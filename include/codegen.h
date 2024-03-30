#ifndef CODE_GEN_H
#define CODE_GEN_H

#include "vm.h"
#include "ast_node.h"
#include "constant_table.h"

#include <stdio.h>
#define INT_TAG 0
#define DOUBLE_TAG 1
#define BLOCK_TAG 2
#define FUNC_TAG 3


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
}Compiler;

void compile_literal(ASTNode* node, Compiler* compiler, SymbolTable* syms);
void compile_binary_expr(ASTNode* node, Compiler* compiler, SymbolTable* syms);
void compile_closure(ASTNode* node, Compiler* compiler, SymbolTable* syms);
void compile_block_expr(ASTNode*, Compiler* compiler, SymbolTable* syms);
void compile_func_call(ASTNode* node, Compiler* compiler, SymbolTable* syms);
void compile_expr(ASTNode*, Compiler* compiler, SymbolTable* syms);
void compile_function(ASTNode* node, Compiler* compiler, SymbolTable* syms);
void compile_statement(ASTNode* node, Compiler* compiler, SymbolTable* syms);
void compile_program(ASTNode* node, Compiler* compiler, SymbolTable* syms);
void write_compiler_data(char* filename, Compiler* compiler, SymbolTable* syms);
void init_compiler(Compiler* compiler);
void free_compiler(Compiler* compiler);

#endif
