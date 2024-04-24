#ifndef SCOPE_H
#define SCOPE_H

#define MAX_STACK_SIZE 256

#define PEEK_STACK_TOP (scopes) (scopes.stack[scopes.sp])
#define PEEK_STACK_AT (scopes, i) (scopes.stack[i])

#include "symbol_table.h"

typedef struct ScopeStack {
    SymbolTable* stack[MAX_STACK_SIZE];
    int sp;
}ScopeStack;

void push_scope(ScopeStack* scopes, SymbolTable* syms);
SymbolTable* pop_scope(ScopeStack* scopes);
Symbol* lookup_symbol(ScopeStack* scopes, char* key, int ksize);
void insert_top(ScopeStack* scopes, char* key, Symbol* value, int ksize);

#endif
