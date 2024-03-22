#include "vector.h"

typedef enum ConstType {
    INT_CONST,
    DOUBLE_CONST,
    STRING_CONST,
    FUNC_CONST,
}ConstType;

typedef struct Const {
    union {
        char* as_func_const;
        char* as_string_const;
        int as_int_const;
        double as_double_const;
    };
    uint16_t const_index;
    ConstType type;
}Const;

typedef Vector ConstTable;

#define ALLOC_CONST(ptr) (ALLOCATE(ptr, Const, 1))

void insert_const(ConstTable* table, Const* c);
Const* lookup_int_const(ConstTable* table, int n);
Const* lookup_double_const(ConstTable* table, double n);
Const* lookup_string_const(ConstTable* table, char* str);
