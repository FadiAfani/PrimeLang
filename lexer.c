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
#define READ_CHAR(lexer) (lexer->src[lexer->cursor])


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
    INIT_VECTOR(lexer->tokens, Token);

    lexer->pos.row = 1;
    lexer->pos.col = 1;
    lexer->filename = NULL;
    lexer->src = NULL;
    lexer->cursor = 0;
    
}

void print_lexer(Lexer lexer) {
    print_position(lexer.pos);
    printf("cursor: %d\n", lexer.cursor);
    printf("num_tokens: %d\n", lexer.tokens->size);
    for (int i = 0; i < lexer.tokens->size; i++) {
        printf("------------------\n");
        print_token(( (Token*) lexer.tokens->arr)[i]);
        printf("------------------\n");
    }
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
            lexer->pos.col = 1;
            break;
        case '\t':
            lexer->pos.col += 4;
            break;
        case ' ':
            lexer->pos.col++;
            break;
        default: return false;
    }
    lexer->cursor++;
    return true;
}

// consumes whitespace until there are none
static inline void skip_whitespace(Lexer* lexer) {
    while(read_whitespace(lexer));
}

static Token tokenize_id(Lexer* lexer) {
    Vector* id;
    INIT_VECTOR(id, char);
    Token tok;
    char c;
    while ((c = READ_CHAR(lexer)) != '\0' && isalpha(c)) {
        lexer->cursor++;
        APPEND(id, c, char);
    }
    ((char*) id->arr)[id->size] = '\0';
    // TODO: change this to a hash table
    const char* value = (const char*) id->arr;
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
    lexer->pos.col += id->size;

    return tok;
}

static Token tokenize_int(Lexer* lexer) {
    Vector* num;
    INIT_VECTOR(num, char);
    char c;
    while((c = READ_CHAR(lexer)) != '\0' && isdigit(c)) {
        lexer->cursor++;
        APPEND(num, c, char);
    }
    ((char*) num->arr)[num->size] = '\0';
    Token tok;
    tok.pos = lexer->pos;
    tok.type = TYPE_INT;
    tok.value = num;
    lexer->pos.col += num->size;

    return tok;
}


void tokenize(Lexer* lexer) {
    char c;
    while((c = READ_CHAR(lexer)) != '\0') {
        Token tok;
        tok.index = lexer->cursor;
        tok.pos = lexer->pos;
        switch(c) {
            case '+':
                lexer->cursor++;
                lexer->pos.col++;
                if (READ_CHAR(lexer) == '=') {
                    tok.type = PLUS_EQ;
                    lexer->cursor++;
                    lexer->pos.col++;
                } else {
                    tok.type = PLUS;
                } 
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;
            case '-':
                lexer->cursor++;
                lexer->pos.col++;
                if (READ_CHAR(lexer) == '=') {
                    tok.type = MINUS_EQ;
                    lexer->cursor++;
                    lexer->pos.col++;
                } else {
                    tok.type = MINUS;
                } 
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;
            case '*':
                lexer->cursor++;
                lexer->pos.col++;
                if (READ_CHAR(lexer) == '=') {
                    tok.type = MULT_EQ;
                    lexer->cursor++;
                    lexer->pos.col++;
                } else {
                    tok.type = MULT;
                } 
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;
            case '/':
                lexer->cursor++;
                lexer->pos.col++;
                if (READ_CHAR(lexer) == '=') {
                    tok.type = DIV_EQ;
                    lexer->cursor++;
                    lexer->pos.col++;
                } else {
                    tok.type = DIV;
                } 
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;
            case ';':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = SEMICOLON;
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;
            case ':':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = COLON;
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;
            case ',':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = COMMA;
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;
            case '(':
                lexer->cursor++;
                lexer->pos.col++;
                if (READ_CHAR(lexer) == ')') {
                    tok.type = UNIT;
                    lexer->cursor++;
                    lexer->pos.col++;
                } else {
                    tok.type = LPAREN;
                }
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;
            case ')':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = RPAREN;
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;
            case '{':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = LCURLY;
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;
            case '}':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = RCURLY;
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;
            case '[':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = LBRAC;
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;
            case ']':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = RBRAC;
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;
            case '.':
                lexer->cursor++;
                lexer->pos.col++; 
                tok.type = DOT;
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;

            case '!':
                lexer->cursor++;
                lexer->pos.col++;
                if (READ_CHAR(lexer) == '=') {
                    tok.type = NEQ;
                    lexer->cursor++;
                    lexer->pos.col++;
                } else {
                    tok.type = NOT;
                }
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;

            case '%':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = MOD;
                tok.value = NULL;
                break;
                APPEND(lexer->tokens, tok, Token);

            case '_':
                lexer->cursor++;
                lexer->pos.col++;
                tok.type = UNDERSCORE;
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;

            case '=':
                lexer->cursor++;
                lexer->pos.col++;
                if (READ_CHAR(lexer) == '=') {
                    tok.type = DEQ;
                    lexer->cursor++;
                    lexer->pos.col++;
                } else {
                    tok.type = EQ;
                }
                tok.value = NULL;
                APPEND(lexer->tokens, tok, Token);
                break;

            default:
                if (isdigit(c)) {
                    tok = tokenize_int(lexer);
                    if (READ_CHAR(lexer) != '.') break;
                    lexer->cursor++;
                    Token frac = tokenize_int(lexer);
                    strcat(tok.value->arr, ".");
                    strcat(tok.value->arr, frac.value->arr);
                    APPEND(lexer->tokens, tok, Token);
                } else if (isalpha(c)) {
                    tok = tokenize_id(lexer);
                    APPEND(lexer->tokens, tok, Token);
                } else if (is_whitespace(c)) {
                    skip_whitespace(lexer); 
                } else {
                    fprintf(stderr, "Unrecognized Symbol: %d", c);
                    exit(EXIT_FAILURE);
                }
                break;
        }
    }
}

void main(int argc, char** argv) {
    Lexer* lexer = init_lexer();
    load_file_into_memory(lexer, argv[1]);
    tokenize(lexer);
    print_lexer(*lexer);
}
