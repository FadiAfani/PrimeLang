#ifndef SYMBOL_H
#define SYMBOL_H

#include "vector.h"

#define LOAD_FACTOR 0.8

typedef enum SymbolType {
    SYMBOL_TYPE,
    SYMBOL_VARIABLE,
    SYMBOL_FUNCTION,

}SymbolType;

/*
 * enums[i] is the enum
 * inner_types[i] is a vector
 * */

typedef struct TypeSymbol {
    Vector enums;
    Vector** inner_types; 
}TypeSymbol;

typedef struct Symbol {
    char* key;
    union {
        TypeSymbol as_type_symbol;
    };
    SymbolType type;
}Symbol;

typedef struct SymbolTable SymbolTable;

struct SymbolTable {
    Vector entries;
    SymbolTable* parent;
};

Symbol* lookup(SymbolTable* table, char* key);
void insert(SymbolTable* table, char* key, Symbol* value);

#endif
