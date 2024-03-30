#ifndef SEMANTICS_H
#define SEMANTICS_H

#include "parser.h"
#include "type.h"


void infer_literal_type(SymbolTable* table, Vector* type_errors, ASTNode* node);
void infer_binary_expr_type(SymbolTable* table, Vector* type_errors, ASTNode* node);
void infer_unary_expr_type(SymbolTable* table, Vector* type_errors, ASTNode* node);
void infer_expr_type(SymbolTable* table, Vector* type_errors, ASTNode* node);
void infer_list_type(SymbolTable* table, Vector* type_errors, ASTNode* node);
void infer_var_from_assignment(SymbolTable* table, Vector* type_errors, ASTNode* node);
void infer_block_type(SymbolTable* table, Vector* type_errors, ASTNode* node);
void infer_statement(SymbolTable* table, Vector* type_errors, ASTNode* node);
void infer_program(SymbolTable* table, Vector* type_errors, ASTNode* node);

#endif
