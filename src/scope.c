#include "../include/scope.h"

void push_scope(ScopeStack* scopes, SymbolTable* table) {
    if (scopes->sp < MAX_STACK_SIZE - 1) {
        scopes->stack[++scopes->sp] = table;
    }
}

SymbolTable* pop_scope(ScopeStack* scopes) {
    SymbolTable* t = scopes->stack[scopes->sp];
    scopes->stack[scopes->sp--] = NULL;
    return t;

}

Symbol* lookup_symbol(ScopeStack* scopes, char* key, int ksize) {
    Symbol* sym = NULL;
    int i;
    for (i = scopes->sp; i >= 0; i--) {
        sym = lookup(&scopes->stack[i]->ht, key, ksize);
        if (sym != NULL) break;
    }

    if (i < scopes->sp && sym != NULL)
        sym->outer_index = scopes->outers++;

    return sym;
}


void insert_top(ScopeStack* scopes, char* key, Symbol* value, int ksize) {
    SymbolTable* top = scopes->stack[scopes->sp];
    if (NULL == lookup_symbol(scopes, key, ksize)) {
        value->local_index = top->locals_count++;
        value->outer_index = -1;
        insert(&top->ht, key, value, ksize);
    } 
}

Symbol* lookup_top(ScopeStack* scopes, char* key, int ksize) {
    SymbolTable* top = scopes->stack[scopes->sp];
    return lookup(&top->ht, key, ksize);
}
