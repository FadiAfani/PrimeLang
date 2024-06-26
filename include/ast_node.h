#ifndef AST_NODE_H
#define AST_NODE_H

#include "token.h"
#include "symbol_table.h"
#include "llist.h"

#define ALLOC_NODE(ptr) (ALLOCATE(ptr, ASTNode, 1))


typedef enum NodeType {
    BIN_EXPR,
    UNARY_EXPR,
    LIST_EXPR,
    LIST_INDEX_EXPR,
    IF_EXPR,
    FOR_EXPR,
    WHILE_EXPR,
    MATCH_EXPR,
    RANGE_EXPR,
    FUNC_CALL_EXPR,
    LITERAL_EXPR,
    TUPLE_EXPR,
    STRUCT_EXPR,
    STRUCT_FIELD_EXPR,
    LAMBDA_EXPR,
    BLOCK_EXPR,
    COMPOUND_STMT,
    BREAK_EXPR,
    STURCT_DECL,
    FUNC_DECL, 
    TYPE_DECL,
    PREDEFINED_TYPE,
    PROGRAM,


}NodeType;

typedef struct TypeList TypeList;

typedef struct IdentifierExpr {
    Token* id_token;
    Vector* id_type;
    PrimeType* p_type;
}IdentifierExpr;

typedef struct BinExpr {
    Token* op;
    ASTNode* left;
    ASTNode* right;
}BinExpr;

typedef struct UnExpr {
    Token* op;
    ASTNode* operand;
}UnExpr;

typedef struct ListExpr {
    Position start_pos;
    Position end_pos;
    Vector items;
}ListExpr;

typedef struct ListIndexExpr {
    ASTNode* index;
    ASTNode* list_id;
}ListIndexExpr;

typedef struct BlockExpr {
    Vector statements;
    SymbolTable table;
}BlockExpr;

typedef struct IfExpr {
    ASTNode* cond;
    ASTNode* expr;
    Vector else_ifs;
    ASTNode* else_expr;
}IfExpr;

typedef struct ForExpr {

}ForExpr;

typedef struct WhileExpr {

}WhileExpr;

typedef struct GroupedExpr {

}GroupedExpr;

typedef struct RangeExpr {

}RangeExpr;




typedef struct FuncCallExpr {
    Token* func_id;
    Arg* arg_list;
    int argc;

}FuncCallExpr;

typedef struct BreakExpr {
    ASTNode* expr;
    NodeType expr_type;
}BreakStmt;

/*
 * enums[i] is the enum
 * inner_types[i] is a vector of tokens
 * */
typedef struct TypeDecl {
    Token* sym_id;
    Vector enums;
    Vector inner_types; 
}TypeDecl;

typedef struct FuncDecl {
    Token* sym_id;
    ASTNode* block;
    Param* params;
}FuncDecl;


struct ASTNode {
    union {
        IdentifierExpr as_id_literal;
        BinExpr as_bin_expr;
        UnExpr as_un_expr;
        ListExpr as_list_expr;
        FuncCallExpr as_func_call;
        Token* as_type;
        Token* as_literal_expr;
        Vector as_compound_statements;
        IfExpr as_if_expr;
        ForExpr as_for_expr;
        WhileExpr as_while_expr;
        ListIndexExpr as_list_index_expr;
        BlockExpr as_block_expr;
        TypeDecl as_type_decl;
        FuncDecl as_func_decl;
        ASTNode* as_break_expr;
    };
    Token* start_tok;
    Token* end_tok;
    NodeType type;
    PrimeType* p_type;
};



void init_block(ASTNode* node);
void init_func_decl(ASTNode* node);

#endif
