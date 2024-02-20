#include "lexer.h"
#include "vector.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#define INIT_TOKEN_SIZE 2048
#define INIT_ID_TOKEN_SIZE 16
#define READ_CHAR(lexer) (lexer->src[lexer->cursor++])


Lexer* init_lexer() {
    Lexer* lexer; 
    ALLOCATE(lexer, Lexer, 1);
    ALLOCATE(lexer->tokens, Vector, 1);

    lexer->pos.row = 1;
    lexer->pos.col = 1;
    lexer->file_name = NULL;
    lexer->src = NULL;
    
}

static iniline bool is_whitespace(char c) {
    switch (c) {
        case '\n':
        case '\t':
        case ' ':
        case '\r':
            return true;
    }
    return false;
}

static inline bool read_whitespace(Lexer* lexer) {
    switch (READ_CHAR(lexer)) {
        case '\r';
        case '\n':
            lexer->pos.row++;
            break;
        case '\t':
            lexer->pos.col += 4;
            break;
        case ' ':
            lexer->pos.col++;
            break;
        default: return false;
    }
    return true;
}

// consumes whitespace until there are none
static inline void skip_whitespace(Lexer* lexer) {
    while(read_whitespace(lexer));
}

static void tokenize_id(Lexer* lexer) {
    Vector* id = init_vector();
    char c;
    while ((c = READ_CHAR(lexer)) && isalpha(c)) {
        append(id, c);
    }

    Token* tok;
    ALLOCATE(tok, Token, 1);
    tok->pos = lexer->pos;
    tok->type = IDENTIFIER;
    tok->value = id->arr;
    free(id);

    append(lexer->tokens, tok);

}


