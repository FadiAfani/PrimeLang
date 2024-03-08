#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "symbol.h"

#define INIT_SYMBOL_TABLE(table) ({ \
    INIT_VECTOR(table.entries, Symbol); \
    table.parent = NULL; \
})

typedef struct SymbolTable SymbolTable;

struct SymbolTable {
    Vector entries;
    SymbolTable* parent;
};

Symbol* lookup(SymbolTable* table, char* key);
void insert(SymbolTable* table, char* key, Symbol* value);
void print_symbol(Symbol* symbol);
void print_symbol_table(SymbolTable* table);

#endif