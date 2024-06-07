#include <stdlib.h>
#include <stdio.h>

#include "../include/vm.h"
#include "../include/value.h"
#include "../include/memory.h"

#define DIS_FLAG

VM* init_VM() {
    VM* vm;
    ALLOCATE(vm, VM, 1);
    vm->fp = -1;
    vm->ip = 0;
    vm->sp = -1;
    
    /* native functions */
    NativeFuncObj native_print_int;
    native_print_int.obj = (Obj){OBJ_NATIVE};
    native_print_int.arity = MAX_ARITY;

    NativeFuncObj native_time;
    native_time.obj = (Obj) {OBJ_NATIVE};
    native_time.arity = 0;

    vm->nativeObjs[0] = native_print_int;
    vm->nativeObjs[1] = native_time;

    /*init heap area */
    INIT_VECTOR(vm->heap, Value);
    return vm;
}

void run(VM* vm) {
    for (;;) {
        //printf("INST: %d\n", vm->code[vm->ip]);
        switch (READ_BYTE(vm))
        {
        case OP_HALT:
            #ifdef DIS_FLAG
            printf("OP_HALT\n");
            #endif
            return;


        case OP_CONST:
        {
            uint16_t index = READ_16(vm);
            Value val = vm->mem[index];
            PUSH(vm, val);

            #ifdef DIS_FLAG
            printf("OP_CONST %d\n", index);
            #endif

            break;
            
        }

        case OP_STOREL:
        {
            Value val = POP(vm);
            uint16_t addr = READ_16(vm);
            int fs = vm->stack[vm->fp].as_int;
            vm->stack[vm->fp - fs + addr + 1] = val; 

            #ifdef DIS_FLAG
            printf("OP_STOREL %d\n", addr);
            #endif

            break;
        }

        case OP_STOREH:
        {
            Value v = POP(vm);
            uint16_t addr = READ_16(vm);
            INSERT_AT(vm->heap, v, Value, addr);
            break;
        }
            
        case OP_ADD_I:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            PUSH(vm, ((Value) {.as_int = a.as_int + b.as_int}));

            #ifdef DIS_FLAG
            printf("OP_ADD_I\n");
            #endif

            break;
        }

        case OP_ADD_F:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            PUSH(vm, ((Value) {.as_double = a.as_double+ b.as_double}));

            #ifdef DIS_FLAG
            printf("OP_ADD_F\n");
            #endif

            break;
        }

        case OP_SUB_I:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            PUSH(vm, ((Value) {.as_int = b.as_int - a.as_int}));

            #ifdef DIS_FLAG
            printf("OP_SUB_I\n");
            #endif

            break;
        }

        case OP_SUB_F:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            PUSH(vm, ((Value) {.as_double = b.as_double - a.as_double}));

            #ifdef DIS_FLAG
            printf("OP_SUB_F\n");
            #endif

            break;
        }
        case OP_MULT_I:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            PUSH(vm, (Value) {.as_int = b.as_int * a.as_int});

            #ifdef DIS_FLAG
            printf("OP_MULT_I\n");
            #endif

            break;    
        }
        case OP_DIV_I:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            PUSH(vm, (Value) {.as_int = b.as_int / a.as_int});

            #ifdef DIS_FLAG
            printf("OP_DIV_I\n");
            #endif

            break;
        }
        case OP_DIV_F:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            PUSH(vm, (Value) {.as_double = b.as_double / a.as_double});

            #ifdef DIS_FLAG
            printf("OP_DIV_F\n");
            #endif

            break;
        }
        case OP_CALL:
        {
            Value label_ptr = POP(vm);
            FuncObj* funcRef = (FuncObj*) label_ptr.as_ref;
            int frame_size = funcRef->loc_cnt + 5; // 3 = as_int + frame_size + prev_vm_loc
            int prev_fp = vm->fp;
            vm->fp = vm->sp - funcRef->arity + frame_size;
            vm->sp += funcRef->loc_cnt - funcRef->arity; /* arguments have already been pushed by now */
            PUSH(vm, ((Value) {.as_int = prev_fp})); /* push the previous return address */
            PUSH(vm, ((Value) {.as_ref = funcRef}));
            PUSH(vm, ((Value) {.as_ref = vm->code}));
            PUSH(vm, ((Value) {.as_int = vm->ip})); // push the return address 
            PUSH(vm, ((Value) {.as_int = frame_size})); // push frame size
            vm->code = funcRef->insts;
            vm->ip = 0;

            #ifdef DIS_FLAG
            printf("OP_CALL\n");
            #endif

            break;
        }
        case OP_CALL_NATIVE:
        {
            uint8_t addr = READ_BYTE(vm);
            uint8_t args_cnt = READ_BYTE(vm);
            Value v;
            switch(addr) {
                case NATIVE_PRINT:
                    for (uint8_t i = 0; i < args_cnt; i++) {

                        switch(READ_BYTE(vm)) {
                            case NATIVE_PRINT_INT:
                                v = POP(vm);
                                printf("%d", v.as_int);
                                break;
                            case NATIVE_PRINT_DOUBLE:
                                v = POP(vm);
                                printf("%lf", v.as_double);
                                break;

                            case NATIVE_PRINT_STRING:
                                v = POP(vm);
                                printf("%s", (char*) v.as_ref);
                                break;
                            default:
                                printf("not a known print variant \n");
                                exit(EXIT_FAILURE);
                        }
                        printf(" ");
                    }
                    printf("\n");
                    break;
                default:
                    printf("not a known native function\n");
                    break;
                    
            }

            #ifdef DIS_FLAG
            printf("OP_CALL_NATIVE %d %d\n", addr, args_cnt);
            #endif

            break;
        }

        case OP_RET:
        {

            Value ret_val = POP(vm);
            vm->code = vm->stack[vm->fp - 2].as_ref;
            vm->ip = vm->stack[vm->fp - 1].as_int;
            int fs = vm->stack[vm->fp].as_int;
            FuncObj* funcRef = vm->stack[vm->fp - 3].as_ref;
            vm->sp = vm->fp - fs;
            vm->fp = vm->stack[vm->fp - 4].as_int;
            PUSH(vm, ret_val);

            #ifdef DIS_FLAG
            printf("OP_RET\n");
            #endif

            break;
        }
        case OP_LOADL:
        {
            uint16_t loc_addr = READ_16(vm);
            int fs = vm->stack[vm->fp].as_int;
            PUSH(vm, vm->stack[vm->fp - fs + loc_addr + 1]);

            #ifdef DIS_FLAG
            printf("OP_LOADL %d\n", loc_addr);
            #endif

            break;
        }
        case OP_LOADH:
        {
            uint16_t addr = READ_16(vm);
            Value v = INDEX_VECTOR(vm->heap, Value, addr);
            PUSH(vm, v);
            break;
        }

        case OP_JMP_ABS:
        {
            uint16_t addr = READ_16(vm);
            vm->ip = addr;

            #ifdef DIS_FLAG
            printf("OP_JMP_ABS %d\n", addr);
            #endif

            break;
        }
        case OP_JMP_REL: /* short jump */
        {
            uint16_t addr = READ_16(vm);
            vm->ip += addr;

            #ifdef DIS_FLAG
            printf("OP_JMP_REL %d\n", addr);
            #endif

            break;
        }

        case OP_CMP_I:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            Value res;

            if (b.as_int > a.as_int) {
                res.as_int = -1;
            } else if (b.as_int < a.as_int) {
                res.as_int = 1;
            } else {
                res.as_int = 0;
            }
            PUSH(vm, res);

            #ifdef DIS_FLAG
            printf("OP_CMP_I\n");
            #endif

            break;
        }

        case OP_CMP_F:
        {
            Value a = POP(vm);
            Value b = POP(vm);
            Value res;

            if (b.as_double - a.as_double > 0) {
                res.as_int = 1;
            } else if (b.as_double - a.as_double < 0) {
                res.as_int = -1;
            } else {
                res.as_int = 0;
            }
            PUSH(vm, res);


            #ifdef DIS_FLAG
            printf("OP_CMP_F\n");
            #endif

            break;
        }

        case OP_EQ:
        {
            Value res = POP(vm);
            if (res.as_int == 0) {
                PUSH(vm, ((Value) {.as_bool = true }));
            }

            #ifdef DIS_FLAG
            printf("OP_EQ\n");
            #endif
            break;

        }

        case OP_BT:
        {
            Value res = POP(vm);
            PUSH(vm, ((Value) {.as_bool = res.as_int == -1 }));

            #ifdef DIS_FLAG
            printf("OP_BT\n");
            #endif

            break;

        }

        case OP_BTE:
        {
            Value res = POP(vm);
            PUSH(vm, ((Value) {.as_bool = res.as_int == -1 || res.as_int == 0}));

            #ifdef DIS_FLAG
            printf("OP_BTE\n");
            #endif

            break;

        }

        case OP_LT:
        {
            Value res = POP(vm);
            PUSH(vm, ((Value) {.as_bool = res.as_int == 1}));

            #ifdef DIS_FLAG
            printf("OP_LT\n");
            #endif

            break;

        }

        case OP_LTE:
        {
            Value res = POP(vm);
            PUSH(vm, ((Value) {.as_bool = res.as_int == 1 || res.as_int == 0}));

            #ifdef DIS_FLAG
            printf("OP_LTE\n");
            #endif

            break;

        }


        case OP_COND_JMP_ABS:
        {
            Value cond = POP(vm);
            uint16_t addr = READ_16(vm);
            if (cond.as_bool) vm->ip = addr;

            #ifdef DIS_FLAG
            printf("OP_COND_JMP_ABS %d\n", addr);
            #endif

            break;
        }
        case OP_JMP_REL_FALSE:
        {
            Value cond = POP(vm);
            int16_t addr = READ_16(vm); // signed ints to handle negative jumps
            if (!cond.as_bool) vm->ip += addr;

            #ifdef DIS_FLAG
            printf("OP_JMP_REL_FALSE %d\n", addr);
            #endif

            break;
        }
        case OP_JMP_REL_TRUE:
        {
            Value cond = POP(vm);
            int16_t addr = READ_16(vm); // signed ints to handle negative jumps
            if (cond.as_bool) vm->ip += addr;

            #ifdef DIS_FLAG
            printf("OP_JMP_REL_TRUE %d\n", addr);
            #endif

            break;
        }
        case OP_INDEX:
        {
           Value idx = POP(vm);
           Value list_ref = POP(vm);
           ListObj* as_list_obj = list_ref.as_ref;
           PUSH(vm, as_list_obj->arr[idx.as_int]);

            #ifdef DIS_FLAG
            printf("OP_INDEX\n");
            #endif

           break;

        }
       
        case OP_SET_LIST:
        {
            Value val = POP(vm);
            Value idx = POP(vm);
            Value list_ref = POP(vm);
            ListObj* as_list_obj = list_ref.as_ref;
            as_list_obj->arr[idx.as_int] = val;
            break;
        }
        case OP_ACCESS_FIELD:
        {
            uint8_t field_num = READ_BYTE(vm);
            StructObj* obj = POP(vm).as_ref;
            PUSH(vm, obj->fields[field_num]);
            break;
        }
        
        case OP_SET_FIELD:
        {
            uint8_t field_num = READ_BYTE(vm);
            StructObj* obj = POP(vm).as_ref;
            Value val_to_set = POP(vm);
            obj->fields[field_num] = val_to_set;
            break;
        }

        default:
            return;
        }

    }
}

