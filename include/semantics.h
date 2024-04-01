#ifndef SEMANTICS_H
#define SEMANTICS_H

#include "parser.h"
#include "type.h"

#define ALLOC_TYPE_CHECKER(ptr) (ALLOCATE(ptr, TypeChecker, 1))

typedef struct TypeChecker {
    Vector semantic_errors;
    ScopeStack scopes;
}TypeChecker;

void init_type_checker(TypeChecker* tc);


void infer_literal_type(TypeChecker* tc, ASTNode* node);
void infer_binary_expr_type(TypeChecker* tc, ASTNode* node);
void infer_unary_expr_type(TypeChecker* tc, ASTNode* node);
void infer_expr_type(TypeChecker* tc, ASTNode* node);
void infer_list_type(TypeChecker* tc, ASTNode* node);
void infer_var_from_assignment(TypeChecker* tc, ASTNode* node);
void infer_block_type(TypeChecker* tc, ASTNode* node);
void infer_statement(TypeChecker* tc, ASTNode* node);
void infer_program(TypeChecker* tc, ASTNode* node);

#endif
