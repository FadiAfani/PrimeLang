#ifndef CODE_GEN_H
#define CODE_GEN_H

#include "vm.h"
#include "ast_node.h"
#include "constant_table.h"

#include <stdio.h>

typedef struct Compiler {
    Vector* data;
    Vector* code;
    ConstTable* consts;
}Compiler;

void compile_literal(ASTNode* node, Compiler* compiler, SymbolTable* syms);
void compile_binary_expr(ASTNode* node, Compiler* compiler, SymbolTable* syms);
void compile_closure(ASTNode* node, Compiler* compiler, SymbolTable* syms);
void compile_block_expr(ASTNode*, Compiler* compiler, SymbolTable* syms);
void compile_func_call(ASTNode* node, Compiler* compiler, SymbolTable* syms);
void compile_expr(ASTNode*, Compiler* compiler, SymbolTable* syms);
void compile_function(ASTNode* node, Compiler* compiler, SymbolTable* syms);
void compile_statement(ASTNode* node, Compiler* compiler, SymbolTable* syms);

void write_compiler_data(char* filename, Compiler* compiler);
void init_compiler(Compiler* compiler);

#endif
