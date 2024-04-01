#ifndef _VALUE_H_
#define _VALUE_H_

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

typedef enum ValueType {
	STRING,
	NUMBER,
    OBJECT,
    RET_ADDR,
    BOOL,
    REFERENCE,
    EMPTY
}ValueType;


typedef union {	
    double as_double;
    int as_int;
    void* as_ref;
    bool as_bool;
}Value;


typedef struct {
    int key;
    Value val;
}HashTableEntry;

typedef struct {
    uint16_t capacity;
    HashTableEntry* table;
}HashTable;

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
