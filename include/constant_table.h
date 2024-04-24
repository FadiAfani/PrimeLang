#include "vector.h"
#include "hash_table.h"

typedef enum ConstType {
    INT_CONST,
    DOUBLE_CONST,
    STRING_CONST,
    FUNC_CONST,
}ConstType;

typedef struct Const {
    union {
        char* as_var_const;
        char* as_func_const;
        char* as_string_const;
        int as_int_const;
        double as_double_const;
    };
    Vector bytes;
    uint16_t const_index;
    ConstType type;
}Const;

typedef HashTable ConstTable;

#define ALLOC_CONST(ptr) (ALLOCATE(ptr, Const, 1))

