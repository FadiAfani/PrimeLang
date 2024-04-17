#include "../include/symbol_table.h"
#include "../include/ast_node.h"
#include <string.h>
#include <stdio.h>


static void rehash_syms(SymbolTable* table) {
    table->entries.size = 0;
    for (size_t i = 0; i < table->entries.capacity; i++) {
        Symbol** arr = table->entries.arr;
        if (arr[i] != NULL) {
            Symbol* sym;
            ALLOC_SYMBOL(sym);
            memcpy(sym, arr[i], sizeof(Symbol));
            //TODO: free symbol
            arr[i] = NULL;
            insert(table, sym->key, sym);
        }
    }
}

static inline float get_load_factor(SymbolTable* table) {
    return (float) table->entries.size / table->entries.capacity;
}

void init_symbol_table(SymbolTable* table) {
    INIT_VECTOR(table->entries, Symbol*);
    table->locals_count = 0;
    table->outers_count = 0;
    for (size_t i = 0; i < table->entries.capacity; i++) {
        Symbol** arr = table->entries.arr;
        arr[i] = NULL;
    }
}

Symbol* lookup(SymbolTable* table, char* key) {
    if (table == NULL) return NULL;
    
    int i = strlen(key) % (table->entries.capacity);
    for (size_t j = i; j < table->entries.capacity; j++) {
        Symbol* sym = INDEX_VECTOR(table->entries, Symbol*, j);
        if (sym == NULL) {
            return NULL;
        }

        if (strcmp(sym->key, key) == 0) {
            return sym;
        }
    }
    return NULL;
}


void insert(SymbolTable* table, char* key, Symbol* value) {
    int i = strlen(key) % (table->entries.capacity);
    value->key = key;
    if (get_load_factor(table) >= LOAD_FACTOR) {
        // refactor
        Vector* entries = &(table->entries);
        size_t old_cap = table->entries.capacity;
        REALLOCATE(entries->arr, table->entries.capacity, Symbol*);
        table->entries.capacity *= SCALE_FACTOR;
        for (size_t j = old_cap; j < table->entries.capacity; j++) {
            Symbol** arr = entries->arr;
            arr[j] = NULL;
        }
        rehash_syms(table);
    }

    size_t j = i;
    size_t idx = j;
    while(INDEX_VECTOR(table->entries, Symbol*, idx) != NULL) {
        j++;
        idx = j % table->entries.capacity;
    }
    value->local_index = table->locals_count++;
    INSERT_AT(table->entries, value, Symbol*, idx);

}

void print_symbol(Symbol* symbol) {
    switch(symbol->type) {
        case SYMBOL_TYPE:
            printf("SYMBOL_TYPE: %s --> ", symbol->key);
            for (size_t i = 0; i < symbol->as_type_symbol.size; i++) {
                printf("Enum: %s ", CAST_VECTOR(INDEX_VECTOR(symbol->as_type_symbol, ASTNode*, i)->as_literal_expr->value, char));
                printf("( ");
                Vector* vec = INDEX_VECTOR(symbol->as_type_symbol, Vector*, i);
                if (vec != NULL) {
                    for (size_t j = 0; j < vec->size; j++) {
                        ASTNode* node = INDEX_VECTOR((*vec), ASTNode*, j);
                        if (node->type == PREDEFINED_TYPE) {
                            printf("%s ", (char*) node->as_type->value.arr);
                        } else {
                            printf("%s ", (char*) node->as_literal_expr->value.arr);
                        }
                    }
                }
                printf(") ");
            }
            printf("\n");
            break;
        case SYMBOL_VARIABLE:
            printf("SYMBOL_VARIABLE: %s\n", symbol->key);
            break;
        case SYMBOL_FUNCTION:
            printf("SYMBOL_FUNCTION: %s\n", symbol->key);
            break;
        default:
            printf("print_symbol: unrecognized symbol \n");
            exit(EXIT_FAILURE);
    }
}


void print_symbol_table(SymbolTable* table) {
    if (table == NULL) return;
    printf("------------------------------------\n");
    for (size_t i = 0; i < table->entries.size; i++) {
        print_symbol(INDEX_VECTOR(table->entries, Symbol*, i));
    }
    printf("------------------------------------\n");
    print_symbol_table(table->parent);
}

