#include "semantics.h"
#include "error.h"
#include <string.h>

static bool compare_types(PrimeType* a, PrimeType* b) {
    if (a->type_kind == BUILT_IN && b->type_kind == BUILT_IN) {
        return a->as_built_in_type == b->as_built_in_type;
    } else if (a->type_kind == USER_DEFINED && b->type_kind == USER_DEFINED) {
        return strcmp(a->as_user_defined_type, b->as_user_defined_type) == 0;
    }
    return false;
}
void infer_literal_type(SymbolTable* table, Vector* type_errors, ASTNode* node) {
    PrimeType* res;
    ALLOC_TYPE(res);
    switch(node->as_literal_expr->type) {
        case INT_LIT:
            res->type_kind = BUILT_IN;
            res->as_built_in_type = INT_TYPE;
            break;
        case DOUBLE_LIT:
            res->type_kind = BUILT_IN;
            res->as_built_in_type = DOUBLE_TYPE;
            break;
        case IDENTIFIER:
            {
                /* search for the identifier in the symbol table
                 * if found simply return its type otherwise return NULL
                 * */

                Symbol* sym = lookup(table, (char*) node->as_literal_expr->value.arr);
                if (sym == NULL) {
                    Error err;
                    err.type = UNDEFINED_SYMBOL_ERROR;
                    err.pos = node->start_tok->pos;
                    err.err_len = node->start_tok->value.size;
                    err.src_start_pos = node->start_tok->index - node->start_tok->pos.col + 1;
                    err.err_msg = "symbol is not defined";

                    APPEND((*type_errors), err, Error);

                }
            }
            break;
        default:
            return;

    }
    node->p_type = res;
}

void infer_var_from_assignment(SymbolTable* table, Vector* type_errors, ASTNode* node) {
    if (node == NULL) return;
    infer_expr_type(table, type_errors, node->as_bin_expr.right);
    node->as_bin_expr.left->as_id_literal.p_type = node->as_bin_expr.right->p_type;
}


void infer_binary_expr_type(SymbolTable* table, Vector* type_errors, ASTNode* node) {
    PrimeType* expr_type;
    ALLOC_TYPE(expr_type);
    Error err;
    err.type = TYPE_ERROR;
    err.pos = node->as_bin_expr.op->pos;
    err.src_start_pos = node->start_tok->index - node->start_tok->pos.col - 1;
    err.err_len = 1; // only point to the operation (without adding '~')
    switch(node->as_bin_expr.op->type) {
        case BT:
        case BTE:
        case LT:
        case LTE:
        case PLUS:
        case MINUS:
        case DIV:
        case MULT:
            infer_expr_type(table, type_errors, node->as_bin_expr.left);
            infer_expr_type(table, type_errors, node->as_bin_expr.right);
            PrimeType* left = node->as_bin_expr.left->p_type;
            PrimeType* right = node->as_bin_expr.right->p_type;

            if (left->type_kind == BUILT_IN && right->type_kind == BUILT_IN) {
                switch(left->as_built_in_type) {
                    case DOUBLE_TYPE:
                    case INT_TYPE:
                        switch(right->as_built_in_type) {
                            case INT_TYPE:
                                expr_type->as_built_in_type = INT_TYPE;
                                break;
                            case DOUBLE_TYPE:
                                expr_type->as_built_in_type = DOUBLE_TYPE;
                                break;
                            default:
                                err.err_msg = "operation is not supported between these types";
                                APPEND((*type_errors), err, Error);
                                return;

                        }
                        break;
                    default:
                        err.err_msg = "operation is not supported between these types";
                        APPEND((*type_errors), err, Error);
                        return;
                }

            } else {
                err.err_msg = "operation is only supported between primitive types";
                APPEND((*type_errors), err, Error);
                return;
            }
        case EQ:
            // check for valid assignment
            left->as_built_in_type = UNIT_TYPE;
            break;
        default:
            printf("type check not implemented\n");
            exit(EXIT_FAILURE);
    }
    node->p_type = expr_type;
}

void infer_unary_expr_type(SymbolTable* table, Vector* type_errors, ASTNode* node) {
    if (node == NULL) return;

    PrimeType* res_t = NULL;
    Error err;
    switch(node->as_un_expr.op->type) {
        case MINUS:
            infer_expr_type(table, type_errors, node->as_un_expr.operand);
            break;
        default:
            err.type = TYPE_ERROR;
            err.src_start_pos = node->as_un_expr.op->index - node->as_un_expr.op->pos.col + 1;
            err.pos = node->as_un_expr.op->pos;
            err.err_len = 1;
            err.err_msg = "operation not supported for this operand type";

            APPEND((*type_errors), err, Error);
            return;
    }
    node->p_type = res_t;
}

void infer_list_type(SymbolTable* table, Vector* type_errors, ASTNode* node) {
    PrimeType* base_type = NULL;
    PrimeType* item_t = NULL;
    for (size_t i = 0; i < node->as_list_expr.items.size; i++) {
        ASTNode* item = INDEX_VECTOR(node->as_list_expr.items, ASTNode*, i);
        infer_expr_type(table, type_errors, item);
        if (i == 0) {
            base_type = item_t;
        } else if (compare_types(item_t, base_type) == false) {
            Error err;
            err.type = TYPE_ERROR;
            err.src_start_pos = node->start_tok->index - node->start_tok->pos.col + 1;
            err.pos = node->start_tok->pos;
            err.err_len = node->end_tok->index - node->start_tok->index + 1;
            err.err_msg = "list is not of a single uniform type, this expression is not compatible with the list's type";
            APPEND((*type_errors), err, Error);
            return;
        }
    }
    PrimeType* list_type;
    ALLOC_TYPE(list_type);
    list_type->as_list_type = item_t;
    list_type->type_kind = LIST_KIND;
    node->p_type = list_type;

}

void infer_block_type(SymbolTable* table, Vector* type_errors, ASTNode* node) {
    return;
}


void infer_expr_type(SymbolTable* table, Vector* type_errors, ASTNode* node) {
    switch(node->type) {
        case LITERAL_EXPR: 
            infer_literal_type(table, type_errors, node);
            break;
        case LIST_EXPR: 
            infer_list_type(table, type_errors, node);
            break;
        case UNARY_EXPR: 
            infer_unary_expr_type(table, type_errors, node);
            break;
        case BIN_EXPR: 
            infer_binary_expr_type(table, type_errors, node);
            break;
       
        default: 
            printf("expression inference not implemented\n");
            break;
    }
}

void infer_statement(SymbolTable* table, Vector* type_errors, ASTNode* node) {
    infer_expr_type(table, type_errors, node);
}

void infer_program(SymbolTable* table, Vector* type_errors, ASTNode* node) {
    for (size_t i = 0; i < node->as_compound_statements.size; i++) {
        infer_statement(table, type_errors, INDEX_VECTOR(node->as_compound_statements, ASTNode*, i));
    }
}

