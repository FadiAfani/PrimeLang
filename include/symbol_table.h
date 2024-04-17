#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "symbol.h"
#include "memory.h"


#define ALLOC_SYMBOL_TABLE(table) (ALLOCATE(table, SymbolTable, 1))

typedef struct SymbolTable SymbolTable;

struct SymbolTable {
    Vector entries;
    SymbolTable* parent;
    int outers_count;
    int locals_count;
};


Symbol* lookup(SymbolTable* table, char* key);
void insert(SymbolTable* table, char* key, Symbol* value);
void print_symbol(Symbol* symbol);
void print_symbol_table(SymbolTable* table);
void init_symbol_table(SymbolTable* table);

#endif
