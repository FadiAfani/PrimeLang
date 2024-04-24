#include "../include/symbol_table.h"
#include "../include/ast_node.h"
#include <string.h>
#include <stdio.h>



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

void init_symbol_table(SymbolTable* st) {
    init_hash_table(&st->ht);
    st->locals_count = 0;
    st->outers_count = 0;
}

