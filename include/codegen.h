#ifndef CODE_GEN_H
#define CODE_GEN_H

#include "vm.h"
#include "ast_node.h"
#include "constant_table.h"
#include "scope.h"

#include <stdio.h>
#define INT_TAG 0
#define DOUBLE_TAG 1
#define BLOCK_TAG 2
#define FUNC_TAG 3
#define STRING_TAG 4 

#define ALLOC_COMPILER(ptr) (ALLOCATE(ptr, Compiler, 1))

typedef enum DataType {
    INT_DATA,
    DOUBLE_DATA,
    BLOCK_DATA,
    FUNC_DATA,
}DataType;

typedef struct Compiler {
    Vector data;
    Vector code;
    ConstTable consts;
    ScopeStack scopes;
}Compiler;

void compile_literal(ASTNode* node, Compiler* compiler);
void compile_binary_expr(ASTNode* node, Compiler* compiler);
void compile_closure(ASTNode* node, Compiler* compiler);
void compile_block_expr(ASTNode*, Compiler* compiler);
void compile_func_call(ASTNode* node, Compiler* compiler);
void compile_assignment(ASTNode* node, Compiler* compiler);
void compile_expr(ASTNode*, Compiler* compiler);
void compile_function(ASTNode* node, Compiler* compiler);
void compile_statement(ASTNode* node, Compiler* compiler);
void compile_program(ASTNode* node, Compiler* compiler);
void compile_if_expr(ASTNode* node, Compiler* compiler);
void write_compiler_data(char* filename, Compiler* compiler);
void init_compiler(Compiler* compiler);
void free_compiler(Compiler* compiler);

#endif
