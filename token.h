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
    EOF_TOK,

}TokenType;

typedef struct Token {
    Position pos;
    TokenType type;
    Vector* value;
    int index; // position in the src array
}Token;

Token* init_token();
void print_position(Position pos);
void print_token(Token token);

