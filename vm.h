#ifndef VM_H
#define VM_H

#define CALL_STACK_CAP 102400 
#define OP_STACK_CAP 64
#define N_NATIVE 2 

#include "value.h"

#include <stdint.h>
#include <stddef.h>

typedef enum OpCode {
	OP_HALT = 0x00,
	OP_CONST = 0x01,
	OP_MEM_SNUM = 0x02, // store number
    OP_MEM_SOBJ = 0x03, // store object
	OP_ADD = 0x04,
	OP_SUB = 0x05,
	OP_MULT = 0x06,
	OP_DIV = 0x07,
    OP_JMP_ABS = 0x08,
    OP_JMP_REL = 0x09,
    OP_COND_JMP_ABS = 0x0A,
    OP_COND_JMP_REL = 0x0B,
    OP_LT_CMP = 0x0C,
    OP_LTE_CMP = 0x0D,
    OP_EQ_CMP = 0x0E,
	OP_RET = 0x0F,
    OP_LOADL = 0x10,
    OP_LOADG = 0x11,
    OP_STOREL = 0x12, 
    OP_STOREG = 0x13,
    OP_CALL = 0x14,
    OP_MKCLSR = 0x15,
    OP_MEM_SLNUM = 0x16,
    OP_MEM_SLOBJ = 0x17,
    OP_LOADP = 0x18,
    OP_JMP_REL_FALSE = 0x19,
    OP_AND = 0x20,
    OP_CALL_NATIVE = 0x23,
    OP_LOAD_OUTER = 0X24,
    OP_STORE_OUTER = 0x25,
    OP_JMP_REL_TRUE = 0X26,
    OP_INDEX = 0x27,
    OP_MK_LIST = 0x28,
    OP_SET_LIST = 0x29,
    OP_APPEND_LIST = 0x30
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
    Value* globals;
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
