#include "parser.h"
#include "memory.h"
#include "vector.h"
#include "error.h"
#include <stdbool.h>

#define READ_TOKEN(parser) (((Token*) (parser->lexer->tokens->arr))[parser->cursor])

static bool consume(Parser* parser, TokenType token_type) {
    Token tok = READ_TOKEN(parser);
    if (tok.type == token_type) {
       parser->cursor++;
       return true;
    } 

    return false;
}

static void parse_identifier_literal(Parser* parser, ASTNode* parent) {
    Token tok = READ_TOKEN(parser);
    if (consume(parser, IDENTIFIER)) {
        APPEND(parent->children, ((ASTNode) {.as_literal_expr = tok, NULL, LITERAL_EXPR}), ASTNode);
    } else {
        APPEND(parser->parsing_errors,
                ((Error) {SYNTAX_ERROR,
                    tok.index, tok.index + tok.value.size,
                    tok.pos, "expected an identifier"}),
                    Error
              );

    }
}

static void parse_int_literal(Parser* parser, ASTNode* parent) {
    Token tok = READ_TOKEN(parser);
    if (consume(parser, TYPE_INT)) {
        APPEND(parent->children, ((ASTNode) {.as_literal_expr = tok, NULL, LITERAL_EXPR}), ASTNode);
    } else {
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
             tok.index, tok.value.size + tok.index,
             tok.pos "expected and int type"}),
            Error
            );
    }
}

static void parse_double_literal(Parser* parser, ASTNode* parent) {
    Token tok = READ_TOKEN(parser);
    if (consume(parser, TYPE_DOUBLE)) {
        APPEND(parent->children, ((ASTNode) {.as_literal_expr = tok, NULL, LITERAL_EXPR}), ASTNode);
    } else {
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
             tok.index, tok.value.size + tok.index,
             tok.pos "expected an int type"}),
            Error
            );
    }
}

static void parse_string_literal(Parser* parser, ASTNode* parent) {
    Token tok = READ_TOKEN(parser);
    if (consume(parser, STRING)) {
        APPEND(parent->children, ((ASTNode) {.as_literal_expr = tok, NULL, LITERAL_EXPR}), ASTNode);
    } else {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
             tok.index, tok.value.size + tok.index,
             tok.pos "expected a string type"}),
            Error
            );
    }

    
}

static void parse_literal_expr(Parser* parser, ASTNode* parent) {
    Token tok = READ_TOKEN(parser);
    switch(tok.type) {
        case IDENTIFIER:
            parse_identifier_literal(parser, parent);
            break;
        case INT_LIT:
            parse_int_literal(parser, parent);
            break;
        case DOUBLE_LIT:
            parse_double_literal(parser, parent);
            break;
        case STRING_LIT: 
            parse_string_literal(parser, parent);
            break;
        case BOOL_LIT:
            break;
        default:
            break;
    }
}

