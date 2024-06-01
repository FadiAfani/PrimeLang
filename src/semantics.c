#include "../include/semantics.h"
#include "../include/error.h"
#include <string.h>
#include <assert.h>

void init_type_checker(TypeChecker* tc) {
    INIT_VECTOR(tc->semantic_errors, Error);
    tc->scopes.sp = -1;
}

static bool compare_types(PrimeType* a, PrimeType* b) {
    if (a->type_kind == BUILT_IN && b->type_kind == BUILT_IN) {
        return a->as_built_in_type == b->as_built_in_type;
    } else if (a->type_kind == USER_DEFINED && b->type_kind == USER_DEFINED) {
        return strcmp(a->as_user_defined_type, b->as_user_defined_type) == 0;
    }
    return false;
}

void infer_literal_type(TypeChecker* tc, ASTNode* node) {
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

                Symbol* sym = lookup_symbol(&tc->scopes, (char*) node->as_literal_expr->value.arr, node->as_literal_expr->value.size);
                if (sym == NULL) {
                    Error err;
                    err.type = UNDEFINED_SYMBOL_ERROR;
                    err.pos = node->start_tok->pos;
                    err.err_len = node->start_tok->value.size;
                    err.src_start_pos = node->start_tok->index - node->start_tok->pos.col + 1;
                    err.err_msg = "symbol is not defined";

                    APPEND(tc->semantic_errors, err, Error);
                    return;

                }
                res = sym->as_var_symbol;
                break;
            }
        default:
            return;

    }
    node->p_type = res;
}


/**
 * checks if pattern is assignable
 * modifies the node accordingly 
*/
static void check_valid_pattern(TypeChecker* tc, ASTNode* node) {
    if (node == NULL) return;
    ASTNode* pat = node->as_bin_expr.left;
    Symbol* sym;
    switch(pat->type) {
        case LITERAL_EXPR:
            if (pat->as_literal_expr->type != IDENTIFIER) {
                /* report pattern error */
                return;
            }
            sym = lookup_symbol(&tc->scopes, (char*) pat->as_literal_expr->value.arr, pat->as_literal_expr->value.size);
            assert(sym != NULL);
            infer_expr_type(tc, node->as_bin_expr.right);
            sym->as_var_symbol = node->as_bin_expr.right->p_type;
            pat->p_type = node->as_bin_expr.right->p_type;

            if (node->as_bin_expr.right->type == FUNC_CALL_EXPR) {
                Symbol* func = lookup_symbol(&tc->scopes, 
                        node->as_bin_expr.right->as_func_call.func_id->value.arr, 
                        node->as_bin_expr.right->as_func_call.func_id->value.size
                );
                int applied_args = node->as_bin_expr.right->as_func_call.argc;
                if (applied_args < func->as_func_symbol.pc) {
                    sym->as_func_symbol.applied_args = node->as_bin_expr.right->as_func_call.arg_list;
                    sym->as_func_symbol.func_type = pat->p_type;
                    sym->as_func_symbol.parent_func = node->as_bin_expr.right->as_func_call.func_id;
                    sym->as_func_symbol.pc = func->as_func_symbol.pc - applied_args;
                    sym->type = SYMBOL_FUNCTION;
                    pat->p_type->type_kind = FUNC_KIND;
                }

            }
            break;

        default:
            printf("unrecognized pattern\n");
            exit(EXIT_FAILURE);
    }
}
void infer_binary_expr_type(TypeChecker* tc, ASTNode* node) {
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
        {
            infer_expr_type(tc, node->as_bin_expr.left);
            infer_expr_type(tc, node->as_bin_expr.right);
            PrimeType* left = node->as_bin_expr.left->p_type;
            PrimeType* right = node->as_bin_expr.right->p_type;

            if (left->type_kind == BUILT_IN && right->type_kind == BUILT_IN) {
                switch(left->as_built_in_type) {
                    case DOUBLE_TYPE:
                    case INT_TYPE:
                        switch(right->as_built_in_type) {
                            case INT_TYPE:
                                expr_type->as_built_in_type = INT_TYPE;
                                expr_type->type_kind = BUILT_IN;
                                break;
                            case DOUBLE_TYPE:
                                expr_type->as_built_in_type = DOUBLE_TYPE;
                                expr_type->type_kind = BUILT_IN;
                                break;
                            default:
                                err.err_msg = "operation is not supported between these types";
                                APPEND(tc->semantic_errors, err, Error);
                                return;

                        }
                        break;
                    default:
                        err.err_msg = "operation is not supported between these types";
                        APPEND(tc->semantic_errors, err, Error);
                        return;
                }

            } else {
                err.err_msg = "operation is only supported between primitive types";
                APPEND(tc->semantic_errors, err, Error);
                return;
            }
            break;
        }
        case EQ:
            // check for valid assignment
            expr_type->type_kind = BUILT_IN;
            expr_type->as_built_in_type = UNIT_TYPE;
            check_valid_pattern(tc, node);
            break;
        default:
            printf("type check not implemented\n");
            exit(EXIT_FAILURE);
    }
    node->p_type = expr_type;
}

void infer_unary_expr_type(TypeChecker* tc, ASTNode* node) {
    if (node == NULL) return;

    PrimeType* res_t = NULL;
    Error err;
    switch(node->as_un_expr.op->type) {
        case MINUS:
            infer_expr_type(tc, node->as_un_expr.operand);
            break;
        default:
            err.type = TYPE_ERROR;
            err.src_start_pos = node->as_un_expr.op->index - node->as_un_expr.op->pos.col + 1;
            err.pos = node->as_un_expr.op->pos;
            err.err_len = 1;
            err.err_msg = "operation not supported for this operand type";

            APPEND(tc->semantic_errors, err, Error);
            return;
    }
    node->p_type = res_t;
}

void infer_list_type(TypeChecker* tc, ASTNode* node) {
    PrimeType* base_type = NULL;
    PrimeType* item_t = NULL;
    for (size_t i = 0; i < node->as_list_expr.items.size; i++) {
        ASTNode* item = INDEX_VECTOR(node->as_list_expr.items, ASTNode*, i);
        infer_expr_type(tc, item);
        if (i == 0) {
            base_type = item_t;
        } else if (compare_types(item_t, base_type) == false) {
            Error err;
            err.type = TYPE_ERROR;
            err.src_start_pos = node->start_tok->index - node->start_tok->pos.col + 1;
            err.pos = node->start_tok->pos;
            err.err_len = node->end_tok->index - node->start_tok->index + 1;
            err.err_msg = "list is not of a single uniform type, this expression is not compatible with the list's type";
            APPEND(tc->semantic_errors, err, Error);
            return;
        }
    }
    PrimeType* list_type;
    ALLOC_TYPE(list_type);
    list_type->as_list_type = item_t;
    list_type->type_kind = LIST_KIND;
    node->p_type = list_type;

}

void infer_block_type(TypeChecker* tc, ASTNode* node) {
    push_scope(&tc->scopes, &node->as_block_expr.table);
    size_t n = node->as_block_expr.statements.size;
    ASTNode* last_expr = INDEX_VECTOR(node->as_block_expr.statements, ASTNode*, n - 1);
    infer_statement(tc, last_expr);
    node->p_type = last_expr->p_type;
    pop_scope(&tc->scopes);
    return;
}

void infer_func_call_type(TypeChecker* tc, ASTNode* node) {
    Arg* cur_arg = node->as_func_call.arg_list;
    ALLOC_TYPE(node->p_type);
    if (strncmp(node->as_func_call.func_id->value.arr, "print", 5) == 0) {
        while (cur_arg != NULL) {
            infer_expr_type(tc, cur_arg->expr);
            cur_arg = cur_arg->next;
        }
        node->p_type->as_built_in_type = UNIT_TYPE;
        return;
    }
    Symbol* sym = lookup_symbol(&tc->scopes, (char*) node->as_func_call.func_id->value.arr, node->as_func_call.func_id->value.size);
    if (sym == NULL) {
        //TODO: report error
        return;
    }

    FuncType* res_t;
    if (sym->as_func_symbol.parent_func != NULL) {

        Symbol* parent = lookup_symbol(&tc->scopes, 
                sym->as_func_symbol.parent_func->value.arr, 
                sym->as_func_symbol.parent_func->value.size
        );

        res_t = parent->as_func_symbol.func_type->as_func_type;
        for (int i = 0; i < node->as_func_call.argc; i++) {
            res_t = res_t->next;
        }
    } else {
        res_t = sym->as_func_symbol.func_type->as_func_type;
    }
    Arg* args = node->as_func_call.arg_list;
    while (args != NULL) {
        ASTNode* p = args->expr;
        infer_expr_type(tc, p);
        res_t = res_t->next;
        args = args->next;
    }
    if (res_t->next == NULL) {
        node->p_type = res_t->pt;
    } else {
        node->p_type->as_func_type = res_t;
    }

}

void infer_if_expr(TypeChecker* tc, ASTNode* node) {
    infer_expr_type(tc, node->as_if_expr.cond);
    infer_expr_type(tc, node->as_if_expr.expr);
    PrimeType* bt = node->as_if_expr.expr->p_type;
    assert(bt != NULL);
    for (size_t i = 0; i < node->as_if_expr.else_ifs.size; i++) {
        ASTNode* elif = INDEX_VECTOR(node->as_if_expr.else_ifs, ASTNode*, i);
        infer_expr_type(tc, elif->as_if_expr.cond);
        infer_expr_type(tc, elif->as_if_expr.expr);
        PrimeType* elif_t = node->as_if_expr.expr->p_type;
        assert(elif_t != NULL);
        if (compare_types(bt, elif_t) == false) {
            //TODO: report a type error 
        }
        bt = elif_t;
    }
    if (node->as_if_expr.else_expr != NULL) {
        infer_expr_type(tc, node->as_if_expr.else_expr);
        if (false == compare_types(bt, node->as_if_expr.else_expr->p_type)) {
            // TODO: report error
            node->p_type = NULL;
        } else {
            node->p_type = bt;
        }
    } else {
        node->p_type = bt;
    }
}


void infer_expr_type(TypeChecker* tc, ASTNode* node) {
    switch(node->type) {
        case LITERAL_EXPR: 
            infer_literal_type(tc, node);
            break;
        case LIST_EXPR: 
            infer_list_type(tc, node);
            break;
        case UNARY_EXPR: 
            infer_unary_expr_type(tc, node);
            break;
        case BIN_EXPR: 
            infer_binary_expr_type(tc, node);
            break;
        case BLOCK_EXPR:
            infer_block_type(tc, node);
            break;
        case IF_EXPR:
            infer_if_expr(tc, node);
            break;
        case FUNC_CALL_EXPR:
            infer_func_call_type(tc, node);
            break;
        default: 
            printf("expression inference not implemented for this node type: %d\n", node->type);
            break;
    }
}

void infer_statement(TypeChecker* tc, ASTNode* node) {
    switch(node->type) {
        case FUNC_DECL:
            infer_block_type(tc, node->as_func_decl.block);
            break;
        default:
            infer_expr_type(tc, node);
            break;
    }
}

void infer_program(TypeChecker* tc, ASTNode* node) {
    for (size_t i = 0; i < node->as_compound_statements.size; i++) {
        infer_statement(tc, INDEX_VECTOR(node->as_compound_statements, ASTNode*, i));
    }
}

