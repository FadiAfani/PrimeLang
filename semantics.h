#ifndef SEMANTICS_H
#define SEMANTICS_H

#include "parser.h"
#include "type.h"


PrimeType* infer_binary_expr_type(ASTNode* node);
PrimeType* infer_unary_expr_type(ASTNode* node);

#endif