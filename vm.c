#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "vm.h"
#include "value.h"
#include "memory.h"
#include "prime_file_parser.h"
#include "hash_table.h"

#define READ_BYTE(vm) (vm->code[vm->ip++])
#define READ_16(vm) (READ_BYTE(vm) << 8 | READ_BYTE(vm))
#define POP(vm) (vm->stack[vm->sp--])
#define PUSH(vm, value) (vm->stack[++(vm->sp)] = value)
#define NATIVE_PRINT 0
#define NATIVE_TIME 1

#define CALL_NATIVE(addr, vm) ({ \
        switch (addr) { \
            case NATIVE_PRINT: \
                call_native_print(vm); \
                break; \
            case NATIVE_TIME: \
                call_native_time(vm); \
                break; \
            default: break; \
        } \
})

static void print_stack(VM* vm) {
    printf("------------------------------------------------\n");
    for (int i = 0; i < vm->sp + 2; i++) {
        Value val = vm->stack[i];
        //printf("sp: %d, fp: %d ", vm->sp, vm->fp);
        switch(val.type) {
            case 0:
                printf("type: STRING, value: %s\n", (char*) vm->stack[i].ref);
                break;
            case 1:
                printf("type: NUMBER, value: %f\n", vm->stack[i].number);
                break;
            case 2:
                printf("type: OBJ, value: %p\n",  vm->stack[i].ref);
                break;
            case 3:
                printf("type: INT, value: %d\n",  vm->stack[i].ret_addr);
            case 5:
                printf("type: REF, value: %p\n", vm->stack[i].ref);
                break;
            default:
                printf("EMPTY\n");
                break;
        }
    }

    printf("------------------------------------------------\n");
}

static void call_native_print(VM* vm) {
    Value arg = POP(vm);
    switch(arg.type) {
        case OBJECT:
            printf("%s\n", ((StringObj*)(arg.ref))->str);
            break;
        case NUMBER:
            printf("%f\n", arg.number);
            break; 
        default:
            printf("%d\n", arg.type);
            break;
    }

}

static inline void call_native_time(VM* vm) {
    clock_t t = clock();
    PUSH(vm, ((Value) {.number = ((double) t)/CLOCKS_PER_SEC, NUMBER}));

}

VM* init_VM() {
    VM* vm;
    ALLOCATE(vm, VM, 1);
    vm->fp = -1;
    vm->ip = 0;
    vm->sp = -1;
    
    /* native functions */
    NativeFuncObj native_print;
    native_print.obj = (Obj){OBJ_NATIVE};
    native_print.arity = 1;

    NativeFuncObj native_time;
    native_time.obj = (Obj) {OBJ_NATIVE};
    native_time.arity = 0;

    vm->nativeObjs[0] = native_print;
    vm->nativeObjs[1] = native_time;
    memset(vm->stack, 0, 512);
    return vm;
}

void run(VM* vm) {
    for (;;) {
        //printf("INST: %d\n", vm->code[vm->ip]);
        switch (READ_BYTE(vm))
        {
        case OP_HALT:
            return;


        case OP_CONST:
        {
            uint16_t index = READ_BYTE(vm) << 8 | READ_BYTE(vm);
            ClosureObj* func = (ClosureObj*)(vm->stack[vm->fp - 3].ref);
            Value val = ((Value*)(func->consts))[index];
            PUSH(vm, val);
            break;
            
        }

        case OP_STOREL:
        {
            Value val = POP(vm);
            uint16_t addr = READ_BYTE(vm) << 8 | READ_BYTE(vm);
            int fs = vm->stack[vm->fp].ret_addr;
            vm->stack[vm->fp - fs + addr + 1] = val; 
            break;

        }
            
        case OP_ADD:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            PUSH(vm, ((Value) {.number = a.number + b.number, NUMBER}));
            //printf("add -> sp: %d, a: %f, b: %f\n", vm->sp, a.number, b.number);
            break;
        }

        case OP_SUB:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            PUSH(vm, ((Value) {.number = b.number - a.number, NUMBER}));
            break;
        }

        case OP_MULT:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            //printf("a: %f\n", a.number);
            //printf("b: %f\n", b.number);
            PUSH(vm, ((Value) {.number = a.number * b.number, NUMBER}));
            break;    
        }
        case OP_DIV:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            PUSH(vm, (Value) {.number = b.number / a.number});
            break;
        }
        case OP_CALL:
        {
            Value label_ptr = POP(vm);
            //printf("start: %d\n", vm->sp);
            ClosureObj* funcRef = (ClosureObj*) label_ptr.ref;
            //printf("sp, fp: %d, %d\n", vm->sp, vm->fp);
            int frame_size = funcRef->loc_cnt + 5; // 3 = ret_addr + frame_size + prev_vm_loc
            int prev_fp = vm->fp;
            vm->fp = vm->sp - funcRef->arity + frame_size;
            vm->sp += funcRef->loc_cnt - funcRef->arity; /* arguments have already been pushed by now */
            PUSH(vm, ((Value) {.ret_addr = prev_fp, RET_ADDR})); /* push the previous return address */
            PUSH(vm, ((Value) {.ref = funcRef, REFERENCE}));
            PUSH(vm, ((Value) {.ref = vm->code, REFERENCE}));
            PUSH(vm, ((Value) {.ret_addr = vm->ip, RET_ADDR})); // push the return address 
            PUSH(vm, ((Value) {.ret_addr = frame_size, RET_ADDR})); // push frame size
            vm->code = funcRef->insts;
            vm->ip = 0;
            //printf("call -> sp: %d, fp: %d, bottom: %d\n", vm->sp, vm->fp, prev_fp);
            break;
        }
        case OP_CALL_NATIVE:
        {
            uint16_t addr = READ_BYTE(vm);
            CALL_NATIVE(addr, vm);
            break;
        }

        case OP_RET:
        {

            Value ret_val = POP(vm);
            vm->code = vm->stack[vm->fp - 2].ref;
            vm->ip = vm->stack[vm->fp - 1].ret_addr;
            int fs = vm->stack[vm->fp].ret_addr;
            ClosureObj* funcRef = vm->stack[vm->fp - 3].ref;
            vm->sp = vm->fp - fs;
            vm->fp = vm->stack[vm->fp - 4].ret_addr;
            PUSH(vm, ret_val);
            //printf("ret -> sp: %d, fp: %d\n", vm->sp, vm->fp);
            break;
        }
        case OP_LOADL:
        {
            uint16_t loc_addr = READ_BYTE(vm) << 8 | READ_BYTE(vm);
            int fs = vm->stack[vm->fp].ret_addr;

            //printf("bot: %d\n", stack_bottom.ret_addr);
            PUSH(vm, vm->stack[vm->fp - fs + loc_addr + 1]);
            break;
        }

        case OP_JMP_ABS:
        {
            uint16_t addr = READ_BYTE(vm) << 8 | READ_BYTE(vm);
            vm->ip = addr;
            break;
        }
        case OP_JMP_REL: /* short jump */
        {
            uint16_t addr = READ_BYTE(vm) << 8 | READ_BYTE(vm);
            vm->ip += addr;
            break;
        }
        case OP_LT_CMP:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            // only supports numbers for now
            a.number < b.number ? PUSH(vm, ((Value) {.as_bool = true, BOOL})) : PUSH(vm, ((Value) {.as_bool = false, BOOL}));
            break;
        }

        case OP_LTE_CMP:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            // only supports numbers for now
            a.number <= b.number ? PUSH(vm, ((Value) {.as_bool = true, BOOL})) : PUSH(vm, ((Value) {.as_bool = false, BOOL}));
            break;
        }

        case OP_EQ_CMP:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            // only supports numbers for now
            //printf("a: %f\n", a.number);
            //printf("b: %f\n", b.number);
            a.number == b.number ? PUSH(vm, ((Value) {.as_bool = true, BOOL})) : PUSH(vm, ((Value) {.as_bool = false, BOOL}));
            break;
        }

        case OP_COND_JMP_ABS:
        {
            Value cond = POP(vm);
            uint16_t addr = READ_BYTE(vm) << 8 | READ_BYTE(vm);
            if (cond.as_bool) vm->ip = addr;
            break;
        }
        case OP_JMP_REL_FALSE:
        {
            Value cond = POP(vm);
            int16_t addr = READ_BYTE(vm) << 8 | READ_BYTE(vm); // signed ints to handle negative jumps
            if (!cond.as_bool) vm->ip += addr;
            break;
        }
        case OP_JMP_REL_TRUE:
        {
            Value cond = POP(vm);
            int16_t addr = READ_BYTE(vm) << 8 | READ_BYTE(vm); // signed ints to handle negative jumps
            if (cond.as_bool) vm->ip += addr;
            break;
        }
        case OP_LOAD_OUTER:
        {
            uint8_t depth = READ_BYTE(vm);
            uint16_t addr = READ_16(vm);
            ClosureObj* cur = vm->stack[vm->fp - 3].ref;

            for (uint8_t i = 0; i < depth; i++) {
                cur = cur->parent;
            }

            Value outer_val = lookup(cur->outerVals, addr);
            //printf("arity: %d\n", outer_val.type);
            PUSH(vm, outer_val);
            break;
        }
        case OP_STORE_OUTER:
        {
            uint8_t depth = READ_BYTE(vm);
            uint16_t addr = READ_16(vm);
            ClosureObj* cur = vm->stack[vm->fp - 3].ref;
            
            for(uint8_t i = 0; i < depth; i++) {
                cur = cur->parent;
            }
            Value val = POP(vm);
            insert(cur->outerVals, addr, val);
            break;
        }
        case OP_MK_LIST:
        {
            uint16_t len = READ_16(vm);
            ListObj* list = init_list_obj();
            list->list_size = len * LIST_SCALING_FACTOR;
            list->list_len = len;
            Value* arr;
            ALLOCATE(arr, Value, len * LIST_SCALING_FACTOR);
            for (int i = len - 1; i >= 0; i--) {
                arr[i] = POP(vm);
            }
            list->arr = arr;
            PUSH(vm, ((Value) {.ref = list, OBJECT}));
            break;

        } 
        case OP_INDEX:
        {
           Value idx = POP(vm);
           Value list_ref = POP(vm);
           ListObj* as_list_obj = list_ref.ref;
           PUSH(vm, as_list_obj->arr[(int) idx.number]);
           break;

        }
        case OP_APPEND_LIST:
        {
            Value val_to_append = POP(vm);
            Value list_ref = POP(vm);
            ListObj* as_list_obj = list_ref.ref;
            if (as_list_obj->list_len >= as_list_obj->list_size) {
                BUFF_DYN_ARR(as_list_obj->arr, as_list_obj->list_size, Value);
                as_list_obj->list_size *= LIST_SCALING_FACTOR; // double the size of the list
            }
            as_list_obj->arr[as_list_obj->list_len++] = val_to_append;
            break;
        }
        case OP_SET_LIST:
        {
            Value val = POP(vm);
            Value idx = POP(vm);
            Value list_ref = POP(vm);
            ListObj* as_list_obj = list_ref.ref;
            as_list_obj->arr[(int) idx.number] = val;
            break;
        }
        case OP_MK_STRUCT:
        {
            uint8_t n_fields = READ_BYTE(vm);
            StructObj* obj = init_struct_obj();
            Value* fields;
            ALLOCATE(fields, Value, n_fields);
            for (uint8_t i = 0; i < n_fields; i++) {
                fields[i] = POP(vm);
            }

            obj->fields = fields;
            PUSH(vm, ((Value) {.ref = obj, OBJECT}));
            break;

        }
        case OP_ACCESS_FIELD:
        {
            uint8_t field_num = READ_BYTE(vm);
            StructObj* obj = POP(vm).ref;
            PUSH(vm, obj->fields[field_num]);
            break;
        }
        default:
            return;
        }

    }
}

int main(int argc, char** argv) {
    VM* vm = init_VM();
    Value main_obj = parse_file_into_main_closure_obj(argv[1]);
    PUSH(vm, main_obj);
    uint8_t insts[2] = {OP_CALL, OP_HALT};
    vm->code = insts;
    clock_t a = clock();
    run(vm);
    clock_t b = clock();
    printf("benchmark: %f\n", (double)(b - a)/CLOCKS_PER_SEC);
    return 0;
}
