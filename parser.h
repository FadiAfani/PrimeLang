#ifndef PARSER_H
#define PARSER_H

#include "symbol_table.h"
#include "lexer.h"

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
    COMPOUND_STMT,
    ASSIGNMENT_STMT,
    STURCT_DECL,
    FUNC_DECL


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

}ListIndexExpr;

typedef struct IfExpr {

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

typedef struct AssignmentStmt {
    ASTNode* left;
    ASTNode* right;
}AssignmentStmt;


struct ASTNode {
    union {
        BinExpr as_bin_expr;
        UnExpr as_un_expr;
        ListExpr as_list_expr;
        FuncCallExpr as_func_call;
        AssignmentStmt as_assignment_stmt;
        Token as_literal_expr;
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
ASTNode* parse_expr(Parser* parser);
ASTNode* parse_group_expr(Parser* parser);
ASTNode* parse_func_call(Parser* parser);
ASTNode* parse_primary(Parser* parser);
ASTNode* parse_assignment(Parser* parser);
ASTNode* parse_statement(Parser* parser);

void print_node(ASTNode* node, int depth);
void free_ast_node(ASTNode* node);


#endif
