#ifndef VM_H
#define VM_H

#define CALL_STACK_CAP 4096 
#define HEAP_SIZE 102400
#define OP_STACK_CAP 64
#define N_NATIVE 2 

#include "value.h"

#include <stdint.h>
#include <stddef.h>

#define READ_BYTE(vm) (vm->code[vm->ip++])
#define READ_16(vm) (READ_BYTE(vm) | READ_BYTE(vm) << 8)
#define POP(vm) (vm->stack[vm->sp--])
#define PUSH(vm, value) (vm->stack[++(vm->sp)] = value)

#define NATIVE_PRINT 0
#define NATIVE_PRINT_INT 0
#define NATIVE_PRINT_DOUBLE 1
#define NATIVE_PRINT_STRING 2
#define NATIVE_TIME 3

typedef enum OpCode {
	OP_HALT,
	OP_CONST,
    OP_ADD_I,
    OP_ADD_F,
	OP_SUB_I,
    OP_SUB_F,
	OP_MULT_I,
    OP_MULT_F,
	OP_DIV_I,
    OP_DIV_F,
    OP_JMP_ABS,
    OP_JMP_REL,
    OP_COND_JMP_ABS,
    OP_COND_JMP_REL,
    OP_CMP_I,
    OP_CMP_F,
    OP_LT_CMP,
    OP_LTE_CMP,
    OP_EQ_CMP,
	OP_RET,
    OP_LOADL,
    OP_LOADG,
    OP_STOREL, 
    OP_STOREG,
    OP_CALL,
    OP_MKCLSR,
    OP_LOADP,
    OP_JMP_REL_FALSE,
    OP_AND,
    OP_CALL_NATIVE,
    OP_LOAD_OUTER,
    OP_STORE_OUTER,
    OP_JMP_REL_TRUE,
    OP_INDEX,
    OP_MK_LIST,
    OP_SET_LIST,
    OP_APPEND_LIST,
    OP_MK_STRUCT ,
    OP_ACCESS_FIELD,
    OP_SET_FIELD
}OpCode;



// make it dynamic for now
typedef struct DynamicArr {
	Value* values;
	size_t capacity;
	size_t size;
}DynamicArr;

typedef struct VM {
	Value stack[CALL_STACK_CAP];
	Value* mem;
    uint8_t* code;
    NativeFuncObj nativeObjs[N_NATIVE];
    Value* heap_area; /* linked-list that captures locals used by inner closures */
	int fp;
    int ip;
    int sp;
}VM;


VM* init_VM();
void run(VM* vm);

#endif
