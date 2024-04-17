#ifndef TYPE_H
#define TYPE_H

#include "memory.h"
#include "vector.h"
#include "token.h"

typedef enum BuiltInType {
    INT_TYPE,
    DOUBLE_TYPE,
    CHAR_TYPE,
    BOOL_TYPE,
    BYTE_TYPE,
    STRING_TYPE,
    UNIT_TYPE,

}BuiltInType;

typedef enum PrimeTypeKind {
    BUILT_IN, 
    LIST_KIND,
    FUNC_KIND,
    USER_DEFINED,
}PrimeTypeKind;

typedef struct PrimeType PrimeType;

typedef struct FuncType FuncType;


struct PrimeType {
    union {
        BuiltInType as_built_in_type;
        FuncType* as_func_type; // vector of PrimeType*
        PrimeType* as_list_type;
        char* as_user_defined_type;
    };
    PrimeTypeKind type_kind;
};

struct FuncType {
    PrimeType* pt;
    FuncType* next;
};

#define ALLOC_TYPE(ptr) (ALLOCATE(ptr, PrimeType, 1))

void print_type(PrimeType* p_type);
FuncType* get_ret_type(FuncType* ft);
void init_func_type(PrimeType* pt);

#endif
