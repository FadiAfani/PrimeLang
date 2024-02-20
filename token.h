typedef struct Position {
    int row;
    int col;
}Position;

typedef enum TokenType {
    IDENTIFIER,
    LSQR_BRAC,
    RSQR_BRAC,
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
    TYPE_BOOL

}TokenType;

typedef struct Token {
    Position pos;
    TokenType type;
    char* value;
}Token;

Token* init_token();

