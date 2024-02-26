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
    GROUPED_EXPR,
    RANGE_EXPR,
    FUNC_CALL_EXPR,
    LITERAL_EXPR,
    TUPLE_EXPR,
    STRUCT_EXPR,
    STRUCT_FIELD_EXPR,
    LAMBDA_EXPR,
    COMPOUND_STMT,
    STURCT_DECL,
    FUNC_DECL,
    NULL_NODE_TYPE


}NodeType;



typedef struct BinExpr {

}BinExpr;

typedef struct UnExpr {

}UnExpr;

typedef struct ListExpr {

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

}FuncCallExpr;


typedef struct ASTNode {
    union {
        BinExpr as_bin_expr;
        UnExpr as_un_expr;
        ListExpr as_list_expr;
        Token as_literal_expr;
    };
    Vector children;
    NodeType type;
}ASTNode;

typedef struct Parser {
    Lexer* lexer;
    ASTNode* root;
    Vector parsing_errors;
    size_t cursor;
}Parser;
#endif
