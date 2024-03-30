#include "value.h"
#include "memory.h"
#include <stdlib.h>

// refactor - this function should return an empty closure

ClosureObj* init_closure(uint8_t* insts, Value* consts, uint16_t n_locals, uint8_t arity) {
    Obj obj = {OBJ_CLSR};
    ClosureObj* ptr;
    ALLOCATE(ptr, ClosureObj, 1);
    ptr->obj = obj;
    ptr->insts = insts;
    ptr->loc_cnt = n_locals;
    ptr->consts = consts;
    ptr->arity = arity;
    return ptr;
}

StringObj* init_string_obj(char* content, uint16_t len) {
    Obj obj = {OBJ_STRING};
    StringObj* str_obj;
    ALLOCATE(str_obj, StringObj, 1);
    str_obj->obj = obj;
    str_obj->len = len;
    str_obj->str = content;
    return str_obj;
}

ListObj* init_list_obj() {
    Obj obj = {OBJ_LIST};
    ValueType list_type = 0;
    ListObj* ptr;
    ALLOCATE(ptr, ListObj, 1);
    ptr->arr = NULL;
    ptr->list_size = 0;
    ptr->list_len = 0;
    return ptr;
}

StructObj* init_struct_obj() {
    Obj obj = {OBJ_STRUCT};
    StructObj* struct_obj;
    ALLOCATE(struct_obj, StructObj, 1);
    struct_obj->fields = NULL;
    return struct_obj;
}

bool valueIsNull (Value val) {
    return val.type == EMPTY; 
}
