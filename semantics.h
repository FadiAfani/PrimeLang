#ifndef SEMANTICS_H
#define SEMANTICS_H

#include "parser.h"
#include "type.h"


PrimeType* infer_binary_expr_type(SymbolTable* table, Vector* type_errors, ASTNode* node);
PrimeType* infer_unary_expr_type(SymbolTable* table, Vector* type_errors, ASTNode* node);
PrimeType* infer_expr_type(SymbolTable* table, Vector* type_errors, ASTNode* node);
PrimeType* infer_list_type(SymbolTable* table, Vector* type_errors, ASTNode* node);

#endif