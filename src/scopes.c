#include "../include/scope.h"

void push_scope(ScopeStack* scopes, SymbolTable* table) {
    if (scopes->sp < MAX_STACK_SIZE - 1) {
        scopes->stack[++scopes->sp] = table;
    }
}

SymbolTable* pop_scope(ScopeStack* scopes) {
    return scopes->stack[scopes->sp--];
}

Symbol* lookup_symbol(ScopeStack* scopes, char* key) {
    for (int i = scopes->sp; i >= 0; i--) {
        Symbol* sym = lookup(scopes->stack[i], key);
        if (sym != NULL) return sym;
    }

    return NULL;
}

void insert_top(ScopeStack* scopes, char* key, Symbol* value) {
    if (NULL == lookup_symbol(scopes, key)) {
        insert(scopes->stack[scopes->sp], key, value);
    }

}

