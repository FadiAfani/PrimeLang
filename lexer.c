#include "lexer.h"
#include "vector.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <string.h>
#include <stdbool.h>

#define INIT_TOKEN_SIZE 2048
#define INIT_ID_TOKEN_SIZE 16
#define READ_CHAR(lexer) (lexer->src[lexer->cursor++])
#define PEEK_CHAR(lexer) (lexer->src[lexer->cursor])
#define PEEK_NEXT(lexer) (lexer->src[lexer->cursor + 1])


void load_file_into_memory(Lexer* lexer, char* filename) {
    FILE* file = fopen(filename, "r");
    assert(file != NULL);
    fseek(file, 0, SEEK_END);
    long len = ftell(file);
    rewind(file);
    char* src;
    ALLOCATE(src, char, len + 1); // extra slot for eof terminator
    fread(src, 1, len, file);
    fclose(file);
    src[len] = '\0';

    lexer->filename = filename;
    lexer->src = src;

}

Lexer* init_lexer() {
    Lexer* lexer; 
    ALLOCATE(lexer, Lexer, 1);
    ALLOCATE(lexer->tokens, Vector, 1);

    lexer->pos.row = 1;
    lexer->pos.col = 1;
    lexer->filename = NULL;
    lexer->src = NULL;
    lexer->cursor = 0;
    
}

static inline bool is_whitespace(char c) {
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
        case '\r':
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

static Token tokenize_id(Lexer* lexer) {
    Vector* id = init_vector();
    Token tok;
    char c;
    while ((c = READ_CHAR(lexer)) && isalpha(c)) {
        APPEND(id, c, char);
    }

    // TODO: change this to a hash table
    char* value = (char*) id->arr;
    printf("%s\n", value);
    if (strcmp(value, "false") == 0) {
        tok.type = TYPE_BOOL;
    } else if (strcmp(value, "true") == 0) {
        tok.type = TYPE_BOOL;
    } else if (strcmp(value, "if") == 0) {
        tok.type = IF;
    } else if (strcmp(value, "else") == 0) {
        tok.type = ELSE;
    } else if (strcmp(value, "elif") == 0) {
        tok.type = ELIF;
    } else if (strcmp(value, "while") == 0) {
        tok.type = WHILE;
    } else if (strcmp(value, "for") == 0) {
        tok.type = FOR;
    } else {
        tok.type = IDENTIFIER;
    } 
    tok.pos = lexer->pos;
    tok.value = id;

    return tok;
}

static Token tokenize_int(Lexer* lexer) {
    Vector* num = init_vector();
    char c;
    while((c = READ_CHAR(lexer)) && isdigit(c)) {
        APPEND(num, c, char);
    }
    Token tok;
    tok.pos = lexer->pos;
    tok.type = TYPE_INT;
    tok.value = num;

    return tok;
}


void tokenize(Lexer* lexer) {
    char c;
    while((c = READ_CHAR(lexer))) {
        Token tok;
        tok.index = lexer->cursor;
        tok.pos = lexer->pos;
        switch(c) {
            case '+':
                if (PEEK_NEXT(lexer) == '=') {
                    READ_CHAR(lexer);
                    tok.type = PLUS_EQ;
                } else {
                    tok.type = PLUS;
                } 
                tok.value = NULL;
                break;
            case '-':
                if (PEEK_NEXT(lexer) == '=') {
                    READ_CHAR(lexer);
                    tok.type = MINUS_EQ;
                } else {
                    tok.type = MINUS;
                } 
                tok.value = NULL;
                break;
            case '*':
                if (PEEK_NEXT(lexer) == '=') {
                    READ_CHAR(lexer);
                    tok.type = MULT_EQ;
                } else {
                    tok.type = MULT;
                } 
                tok.value = NULL;
                break;
            case '/':
                if (PEEK_NEXT(lexer) == '=') {
                    READ_CHAR(lexer);
                    tok.type = DIV_EQ;
                } else {
                    tok.type = DIV;
                } 
                tok.value = NULL;
                break;
            case ';':
                tok.type = SEMICOLON;
                tok.value = NULL;
                break;
            case ':':
                tok.type = COLON;
                tok.value = NULL;
                break;
            case ',':
                tok.type = COMMA;
                tok.value = NULL;
                break;
            case '(':
                if (PEEK_NEXT(lexer) == ')') {
                    READ_CHAR(lexer);
                    tok.type = UNIT;
                } else {
                    tok.type = LPAREN;
                }
                tok.value = NULL;
                break;
            case ')':
                tok.type = RPAREN;
                tok.value = NULL;
                break;
            case '{':
                tok.type = LCURLY;
                tok.value = NULL;
                break;
            case '}':
                tok.type = RCURLY;
                tok.value = NULL;
                break;
            case '[':
                tok.type = LBRAC;
                tok.value = NULL;
                break;
            case ']':
                tok.type = RBRAC;
                tok.value = NULL;
                break;
            case '.':
                tok.type = DOT;
                tok.value = NULL;
                break;

            case '!':
                if (PEEK_NEXT(lexer) == '=') {
                    READ_CHAR(lexer);
                    tok.type = NEQ;
                } else {
                    tok.type = NOT;
                }
                tok.value = NULL;
                break;

            case '%':
                tok.type = MOD;
                tok.value = NULL;
                break;

            case '_':
                tok.type = UNDERSCORE;
                tok.value = NULL;
                break;

            case '=':
                if (PEEK_NEXT(lexer) == '=') {
                    READ_CHAR(lexer);
                    tok.type = DEQ;
                } else {
                    tok.type = EQ;
                }
                tok.value = NULL;
                break;
            case '\0':
                return;

            default:
                if (isdigit(c)) {
                    tok = tokenize_int(lexer);
                    if (PEEK_CHAR(lexer) != '.') break;
                    READ_CHAR(lexer);
                    Token frac = tokenize_int(lexer);
                    strcat(tok.value->arr, ".");
                    strcat(tok.value->arr, frac.value->arr);
                } else if (isalpha(c)) {
                    tok = tokenize_id(lexer);
                } else if(is_whitespace(c)) {
                    read_whitespace(lexer);
                } else {
                    fprintf(stderr, "Unrecognized Symbol: %d", c);
                    exit(EXIT_FAILURE);
                }
                break;
        }
        APPEND(lexer->tokens, tok, Token);
    }
}

void main(int argc, char** argv) {
    Lexer* lexer = init_lexer();
    load_file_into_memory(lexer, argv[1]);
    tokenize(lexer);

}
