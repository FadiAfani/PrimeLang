#include "../include/value.h"
#include "../include/memory.h"
#include <stdlib.h>

// refactor - this function should return an empty closure



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

