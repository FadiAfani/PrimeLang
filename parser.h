#ifndef PARSER_H
#define PARSER_H

#include "symbol_table.h"
#include "lexer.h"
#include "symbol.h"

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


}NodeType;

typedef struct ASTNode ASTNode;

typedef struct BinExpr {
    Token op;
    ASTNode* left;
    ASTNode* right;
}BinExpr;

typedef struct UnExpr {
    Token op;
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
    ASTNode* final_expr; // if exists, then the block is evaluated otherwise -> ()
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
    Token func_id;
    Vector params;

}FuncCallExpr;

typedef struct BreakExpr {
    ASTNode* expr;
    NodeType expr_type;
}BreakStmt;

typedef struct SymbolNode {
    ASTNode* sym_id;
    Symbol* symbol;
}SymbolNode;


struct ASTNode {
    union {
        BinExpr as_bin_expr;
        UnExpr as_un_expr;
        ListExpr as_list_expr;
        FuncCallExpr as_func_call;
        Token as_type;
        Token as_literal_expr;
        Vector as_compound_statements;
        IfExpr as_if_expr;
        ForExpr as_for_expr;
        WhileExpr as_while_expr;
        ListIndexExpr as_list_index_expr;
        BlockExpr as_block_expr;
        SymbolNode as_symbol;
    };
    NodeType type;
};

typedef struct Parser {
    Lexer lexer;
    ASTNode* root;
    Vector parsing_errors;
    size_t cursor;
}Parser;

ASTNode* parse_identifier_literal(Parser* parser);
ASTNode* parse_int_literal(Parser* parser);
ASTNode* parse_double_literal(Parser* parser);
ASTNode* parse_string_literal(Parser* parser);
ASTNode* parse_literal(Parser* parser);
ASTNode* parse_factor(Parser* parser);
ASTNode* parse_term(Parser* parser);
ASTNode* parse_and(Parser* parser);
ASTNode* parse_or(Parser* parser);
ASTNode* parse_assignment(Parser* parser);
ASTNode* parse_expr(Parser* parser);
ASTNode* parse_group_expr(Parser* parser);
ASTNode* parse_func_call(Parser* parser);
ASTNode* parse_primary(Parser* parser);
ASTNode* parse_statement(Parser* parser);
ASTNode* parse_compound(Parser* parser);
ASTNode* parse_list_index(Parser* parser);
ASTNode* parse_block(Parser* parser);
ASTNode* parse_if(Parser* parser);
ASTNode* parse_elif(Parser* parser);

void print_node(ASTNode* node, int depth);
void free_ast_node(ASTNode* node);


#endif
