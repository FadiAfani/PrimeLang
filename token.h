#ifndef TOKEN_H
#define TOKEN_H

#include "vector.h"

typedef struct Position {
    int row;
    int col;
}Position;

typedef enum TokenType {
    IDENTIFIER,
    LBRAC,
    RBRAC,
    LPAREN,
    RPAREN,
    SEMICOLON,
    COLON,
    COMMA,
    MULT,
    DIV,
    PLUS,
    MINUS,
    EQ,
    DEQ,
    NOT,
    NEQ,
    BT,
    ST,
    BTE,
    STE,
    ARROW,
    LCURLY,
    RCURLY,
    MOD,
    AND,
    OR,
    PLUS_EQ,
    MINUS_EQ,
    MULT_EQ,
    DIV_EQ,
    HASHTAG,
    INT_LIT,
    STRING_LIT,
    DOUBLE_LIT,
    BOOL_LIT,
    TYPE_INT,
    TYPE_DOUBLE,
    TYPE_STRING,
    TYPE_BYTE,
    TYPE_CHAR,
    TYPE_BOOL,
    FALSE,
    TRUE,
    IF,
    ELIF,
    ELSE,
    FOR,
    WHILE,
    UNDERSCORE,
    UNIT,
    DOT,
    QOUT,
    TOK_EOF,
    BREAK,
    THEN,
    RANGE,
    PIPE, // '|'
    KEYWORD_TYPE, // "type" keyword for defining ADTs

}TokenType;

typedef struct Token {
    Position pos;
    TokenType type;
    Vector value;
    size_t index; // position in the src array
}Token;

Token* init_token();
void print_position(Position pos);
void print_token(Token token);

#endif
