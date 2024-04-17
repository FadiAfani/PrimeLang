#include "../include/ast_node.h"

void init_block(ASTNode* node) {
    INIT_VECTOR(node->as_block_expr.statements, ASTNode*);
    INIT_VECTOR(node->as_block_expr.table.entries, Symbol*);
    node->type = BLOCK_EXPR;
}

void init_func_decl(ASTNode* node) {
    ALLOC_NODE(node->as_func_decl.block);
    init_block(node->as_func_decl.block);
    INIT_VECTOR(node->as_func_decl.parameters, ASTNode*);
    node->as_func_decl.sym_id = NULL;
    node->type = FUNC_DECL;
}
