#include "symbol.h"
#include <string.h>

static inline float get_load_factor(SymbolTable* table) {
    return table->entries.size / table->entries.capacity;
}

Symbol* lookup(SymbolTable* table, char* key) {
    int i = strlen(key) % (table->entries.size);
    for (size_t j = i; j < table->entries.size; i++) {
        Symbol* sym = INDEX_VECTOR(table->entries, Symbol*, i);
        if (sym == NULL) return NULL;

        if (strcmp(sym->key, key) == 0) {
            return sym;
        }
    }
    return NULL;
}

void insert(SymbolTable* table, char* key, Symbol* value) {
    int i = strlen(key) % (table->entries.size);
    value->key = key;
    if (get_load_factor(table) >= LOAD_FACTOR) {
        // refactor
        Vector* entries = &(table->entries);
        REALLOCATE(entries->arr, table->entries.capacity, Symbol);
        table->entries.capacity *= 2;
    }
    for (size_t j = i; j < table->entries.size; i++) {
        Symbol* sym = INDEX_VECTOR(table->entries, Symbol*, i);
        if (sym == NULL || strcmp(sym->key, key) == 0) {
            APPEND(table->entries, value, Symbol*);
            break;
        }

    }
}

