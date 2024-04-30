#ifndef SYMBOL_H
#define SYMBOL_H

#include "vector.h"
#include "type.h"
#include "llist.h"

#define LOAD_FACTOR 0.8

#define ALLOC_SYMBOL(ptr) (ALLOCATE(ptr, Symbol, 1))

typedef struct Symbol Symbol;

typedef enum SymbolType {
    SYMBOL_TYPE,
    SYMBOL_VARIABLE,
    SYMBOL_FUNCTION,
    SYMBOL_ENUM,
    SYMBOL_PARAMETER,

}SymbolType;

/**
 * parameters contains keys (char*) to other symbols
 * func_type is the function's type 
*/


typedef struct FuncSymbol {
    Param* param_list;
    Arg* applied_args;
    PrimeType* func_type;
    int pc;
    Token* parent_func;
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
    fs.param_list = NULL; \
    fs.applied_args = NULL; \
    fs.func_type = NULL; \
    fs.parent_func = NULL; \
    fs.pc = 0; \
})


void print_symbol(Symbol* symbol);

#endif
