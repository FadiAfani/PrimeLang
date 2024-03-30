#ifndef _VALUE_H_
#define _VALUE_H_

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#include "vh_common.h"

typedef struct Obj {
    enum {
        OBJ_CLSR,
        OBJ_FUNC,
        OBJ_STRING,
        OBJ_NATIVE,
        OBJ_LIST,
        OBJ_STRUCT
    }ObjType;
    
}Obj;

typedef struct FuncObj FuncObj;

struct FuncObj{
    Obj obj;
    uint8_t arity;
    uint8_t* insts;
    uint16_t loc_cnt;
    Value** outerVals; /* stores references to consts */
    FuncObj* parent; /* a reference to a parent closure */
};


typedef struct {
    Obj obj;
    char* str;
    uint16_t len;
}StringObj;


typedef struct {
    Obj obj;
    uint8_t arity;
}NativeFuncObj;

typedef struct {
    Obj obj;
    ValueType list_type;
    Value* arr;
    size_t list_size;
    size_t list_len;
}ListObj;

typedef struct {
    Obj obj;
    Value* fields;
}StructObj;


FuncObj* init_closure(uint8_t* insts, Value* consts, uint16_t n_locals, uint8_t arity);
StringObj* init_string_obj(char* str, uint16_t len);
ListObj* init_list_obj();
StructObj* init_struct_obj();
bool valueIsNull(Value val);

#endif
