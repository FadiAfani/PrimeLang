#ifndef SYMBOL_H
#define SYMBOL_H

#include "vector.h"
#include "type.h"

#define LOAD_FACTOR 0.8


typedef enum SymbolType {
    SYMBOL_TYPE,
    SYMBOL_VARIABLE,
    SYMBOL_FUNCTION,
    SYMBOL_ENUM,
    SYMBOL_PARAMETER

}SymbolType;

/**
 * parameters contains keys (char*) to other symbols
 * func_type is the function's type 
*/

typedef struct FuncSymbol {
    Vector parameters;
    PrimeType* func_type;
}FuncSymbol;


typedef struct Symbol {
    char* key;
    union {
        PrimeType* as_var_symbol;
        Vector as_type_symbol; // vector of enum keys (char*)
        Vector as_enum_symbol; // vector of PrimeType's
        FuncSymbol as_func_symbol; // vector storing function parameters
    };
    int outer_index; /* -1 if symbol is not an outer value*/
    int local_index;
    SymbolType type;
}Symbol;

#define INIT_FUNC_SYMBOL(fs) ({ \
    INIT_VECTOR(fs.parameters, Symbol*); \
    ALLOC_TYPE(fs.func_type); \
})


#define ALLOC_SYMBOL(ptr) (ALLOCATE(ptr, Symbol, 1))

#endif
