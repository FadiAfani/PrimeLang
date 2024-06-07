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
    for (int i = scopes->sp; i >= 0; i--) {
        Symbol* sym = lookup(&scopes->stack[i]->ht, key, ksize);
        if (sym != NULL) return sym;
    }

    return NULL;
}

void insert_top(ScopeStack* scopes, char* key, Symbol* value, int ksize) {
    SymbolTable* top = scopes->stack[scopes->sp];
    if (NULL == lookup(&top->ht, key, ksize)) {
        value->local_index = top->locals_count++;
        value->outer_index = -1;
        insert(&top->ht, key, value, ksize);
    } else {
        value->outer_index = scopes->outers++;
    }

}
Symbol* lookup_top(ScopeStack* scopes, char* key, int ksize) {
    SymbolTable* top = scopes->stack[scopes->sp];
    return lookup(&top->ht, key, ksize);
}
