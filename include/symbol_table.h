#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "symbol.h"
#include "memory.h"
#include "hash_table.h"


#define ALLOC_SYMBOL_TABLE(table) (ALLOCATE(table, SymbolTable, 1))

typedef struct SymbolTable SymbolTable;

struct SymbolTable {
    HashTable ht;
    int outers_count;
    int locals_count;
};


void print_symbol(Symbol* symbol);
void print_symbol_table(SymbolTable* table);
void init_symbol_table(SymbolTable* table);

#endif
