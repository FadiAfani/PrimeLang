#include "semantics.h"
#include "error.h"



PrimeType* infer_binary_expr_type(SymbolTable* table, Vector* type_errors, ASTNode* node) {
    PrimeType* left;
    PrimeType* right;
    PrimeType* expr_type;
    ALLOCATE(expr_type, PrimeType, 1);
    Error err;
    switch(node->as_bin_expr.op.type) {
        case BT:
        case BTE:
        case LT:
        case LTE:
        case PLUS:
        case MINUS:
        case DIV:
        case MULT:
            left = infer_expr_type(table, type_errors, node->as_bin_expr.left);
            right = infer_expr_type(table, type_errors, node->as_bin_expr.right);
            if (left == NULL || right == NULL) {
                return NULL;
            }

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
                                err.type = TYPE_ERROR;
                                err.err_msg = "'*' operation is not supported between these types";
                                APPEND((*type_errors), err, Error);
                                break;
                        }
                    default:
                        err.type = TYPE_ERROR;
                        err.err_msg = "'*' operation is not supported between these types";
                        APPEND((*type_errors), err, Error);
                        break;
                }

            } else {
                err.type = TYPE_ERROR;
                err.err_msg = "'*' operation is not supported between these types";
                APPEND((*type_errors), err, Error);
            }
        case EQ:
            // check for valid assignment
            left->as_built_in_type = UNIT_TYPE;
            break;
        default:
            printf("type check not implemented\n");
            exit(EXIT_FAILURE);
    }
}

PrimeType* infer_list_type(SymbolTable* table, Vector* type_errors, ASTNode* node) {

}


PrimeType* infer_expr_type(SymbolTable* table, Vector* type_errors, ASTNode* node) {
    return NULL;
}