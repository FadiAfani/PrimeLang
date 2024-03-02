#include "parser.h"
#include "memory.h"
#include "vector.h"
#include "error.h"
#include <stdbool.h>

#define READ_TOKEN(parser) (((Token*) (parser->lexer.tokens.arr))[parser->cursor])
#define PEEK_AT(parser, i) (((Token*) (parser->lexer.tokens.arr))[parser->cursor + i])
#define PEEK_NEXT(parser) (PEEK_AT(parser, 1))


void init_parser(Parser* parser) {
    Lexer lexer;
    init_lexer(&lexer);
    parser->lexer = lexer;
    parser->root = NULL;
    parser->parsing_errors = (Vector) {INIT_VECTOR_CAP, 0, NULL};
    ALLOCATE(parser->parsing_errors.arr, Error, INIT_VECTOR_CAP);
    parser->cursor = 0;
}

void free_ast_node(ASTNode* node) {
    if (node == NULL) return;

    switch(node->type) {
        case LITERAL_EXPR: break;
        case BIN_EXPR:
            free_ast_node(node->as_bin_expr.left);
            free_ast_node(node->as_bin_expr.right);
            break;
        case LIST_EXPR:
            for (size_t i = 0; i < node->as_list_expr.items.size; i++) {
                free_ast_node(INDEX_VECTOR(node->as_list_expr.items, ASTNode*, i));
            }
            break;
        case ASSIGNMENT_STMT:
            free_ast_node(node->as_assignment_stmt.left);
            free_ast_node(node->as_assignment_stmt.right);
            break;
        default:
            printf("free is not implemented for this node type\n");
            exit(EXIT_FAILURE);
    }
    free(node);
}

void print_node(ASTNode* node, int depth) {
    if (node == NULL) return;

    for (int i = 0; i < depth; i++) {
        printf("\t");
    }
    switch(node->type) {
        case LITERAL_EXPR:
            printf("<literal>: %s\n", (char*) node->as_literal_expr.value.arr);
            break;
        case BIN_EXPR:
            printf("<binary-expr>: %s\n", CAST_VECTOR(node->as_bin_expr.op.value, char));
            print_node(node->as_bin_expr.left, depth + 1);
            print_node(node->as_bin_expr.right, depth + 1);
            break;
        case LIST_EXPR:
            printf("<list-expr>\n");
            for (size_t i = 0; i < node->as_list_expr.items.size; i++) {
                print_node(INDEX_VECTOR(node->as_list_expr.items, ASTNode*, i), depth + 1);
            }
            break;
        case TUPLE_EXPR:
            printf("<tuple-expr>\n");
            for (size_t i = 0; i < node->as_list_expr.items.size; i++) {
                print_node(INDEX_VECTOR(node->as_list_expr.items, ASTNode*, i), depth + 1);
            }
            break;
        case ASSIGNMENT_STMT:
            printf("<assignment-statement>\n");
            print_node(node->as_assignment_stmt.left, depth + 1);
            print_node(node->as_assignment_stmt.right, depth + 1);
            break;
        case COMPOUND_STMT:
            printf("<compound-statement>\n");
            for (size_t i = 0; i < node->as_compound_statements.size; i++) {
                print_node(INDEX_VECTOR(node->as_compound_statements, ASTNode*, i), depth + 1);
            }
            break;
        case BLOCK_EXPR:
            printf("<block-expr>\n");
            for (size_t i = 0; i < node->as_compound_statements.size; i++) {
                print_node(INDEX_VECTOR(node->as_compound_statements, ASTNode*, i), depth + 1);
            }
            break;
        case LIST_INDEX_EXPR:
            printf("<list-index>\n");
            print_node(node->as_list_index_expr.index, depth + 1);
            print_node(node->as_list_index_expr.list_id, depth + 1);
            break;
        case IF_EXPR:
            printf("<if-expr>\n");
            print_node(node->as_if_expr.cond, depth + 1);
            print_node(node->as_if_expr.block, depth + 1);
            print_node(node->as_if_expr.else_block, depth + 1);
            for (size_t i = 0; i < node->as_if_expr.else_ifs.size; i++) {
                print_node(INDEX_VECTOR(node->as_if_expr.else_ifs, ASTNode*, i), depth + 1);
            }
            break;

        default:
            printf("print method is not implemented for this node type\n");
            exit(EXIT_FAILURE);

    }
}

static bool consume(Parser* parser, TokenType token_type) {
    Token tok = READ_TOKEN(parser);
    if (tok.type == token_type) {
       parser->cursor++;
       return true;
    } 

    return false;
}

ASTNode* parse_identifier_literal(Parser* parser) {

    Token tok = READ_TOKEN(parser);

    if (consume(parser, IDENTIFIER)) {
        ASTNode* node;
        ALLOCATE(node, ASTNode, 1);
        node->as_literal_expr = tok;
        node->type = LITERAL_EXPR;
        return node;
    } 
    
    APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
                tok.index, tok.index + tok.value.size,
                tok.pos, "expected an identifier"}),
                Error
          );
    return NULL;
}

ASTNode* parse_int_literal(Parser* parser) {
    Token tok = READ_TOKEN(parser);
    if (consume(parser, INT_LIT)) {
        ASTNode* node;
        ALLOCATE(node, ASTNode, 1);
        node->as_literal_expr = tok;
        node->type = LITERAL_EXPR;
        return node;
    } 
    
    APPEND(parser->parsing_errors,
        ((Error) {SYNTAX_ERROR,
         tok.index, tok.value.size + tok.index,
         tok.pos, "expected and int type"}),
        Error
        );
    return NULL;
}

ASTNode* parse_double_literal(Parser* parser) {
    Token tok = READ_TOKEN(parser);
    if (consume(parser, DOUBLE_LIT)) {
        ASTNode* node;
        ALLOCATE(node, ASTNode, 1);
        node->as_literal_expr = tok;
        node->type = LITERAL_EXPR;
        return node;
    } 
    APPEND(parser->parsing_errors,
        ((Error) {SYNTAX_ERROR,
         tok.index, tok.value.size + tok.index,
         tok.pos, "expected an int type"}),
        Error
        );
    return NULL;
}

ASTNode* parse_string_literal(Parser* parser) {
    Token tok = READ_TOKEN(parser);
    if (consume(parser, STRING_LIT)) {
        ASTNode* node;
        ALLOCATE(node, ASTNode, 1);
        node->as_literal_expr = tok;
        node->type = LITERAL_EXPR;
        return node;
    } 

    APPEND(parser->parsing_errors,
        ((Error) {SYNTAX_ERROR,
         tok.index, tok.value.size + tok.index,
         tok.pos, "expected a string type"}),
        Error
        );
    return NULL;
}

ASTNode* parse_literal(Parser* parser) {
    Token tok = READ_TOKEN(parser);
    switch(tok.type) {
        case IDENTIFIER:
            return parse_identifier_literal(parser);
        case INT_LIT:
            return parse_int_literal(parser);
        case DOUBLE_LIT:
            return parse_double_literal(parser);
        case STRING_LIT: 
            return parse_string_literal(parser);
        case BOOL_LIT:
        default:
            return NULL;
    }
}

// <factor> := <literal> ( [ * | \ ] <literal> )*
ASTNode* parse_factor(Parser* parser) {
    ASTNode* node = parse_primary(parser);

    while(true) {
        Token tok = READ_TOKEN(parser);
        if (consume(parser, MULT) || consume(parser, DIV)) {
            ASTNode* new_node;
            ALLOCATE(new_node, ASTNode, 1);
            new_node->as_bin_expr.op = tok;
            new_node->as_bin_expr.left = node;
            new_node->as_bin_expr.right = parse_literal(parser);
            new_node->type = BIN_EXPR;
            node = new_node;
        } else {break;}
    }
    return node;

}

ASTNode* parse_term(Parser* parser) {
    ASTNode* node = parse_factor(parser);

    while(true) {
        Token tok = READ_TOKEN(parser);
        if (consume(parser, PLUS) || consume(parser, MINUS)) {
            ASTNode* new_node;
            ALLOCATE(new_node, ASTNode, 1);
            new_node->as_bin_expr.op = tok;
            new_node->as_bin_expr.left = node;
            new_node->as_bin_expr.right = parse_factor(parser);
            new_node->type = BIN_EXPR;
            node = new_node;
        } else {break;}
    }
    return node;
}

ASTNode* parse_and(Parser* parser) {
    ASTNode* node = parse_term(parser);

    while(true) {
        Token tok = READ_TOKEN(parser);
        if (consume(parser, AND)) {
            ASTNode* new_node;
            ALLOCATE(new_node, ASTNode, 1);
            new_node->as_bin_expr.op = tok;
            new_node->as_bin_expr.left = node;
            new_node->as_bin_expr.right = parse_term(parser);
            new_node->type = BIN_EXPR;
            node = new_node;
        } else {break;}
    }
    return node;
}

ASTNode* parse_or(Parser* parser) {
    ASTNode* node = parse_and(parser);

    while(true) {
        Token tok = READ_TOKEN(parser);
        if (consume(parser, OR)) {
            ASTNode* new_node;
            ALLOCATE(new_node, ASTNode, 1);
            new_node->as_bin_expr.op = tok;
            new_node->as_bin_expr.left = node;
            new_node->as_bin_expr.right = parse_and(parser);
            new_node->type = BIN_EXPR;
            node = new_node;
        } else {break;}
    }
    return node;
}

ASTNode* parse_list(Parser* parser) {

    consume(parser, LBRAC);

    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->as_list_expr.items = (Vector) {INIT_VECTOR_CAP, 0, NULL};
    ALLOCATE(node->as_list_expr.items.arr, ASTNode*, INIT_VECTOR_CAP);
    APPEND(node->as_list_expr.items, parse_expr(parser), ASTNode*);
    node->type = LIST_EXPR;

    while(true) {
        Token tok = READ_TOKEN(parser);
        if (consume(parser, COMMA)) {
            APPEND(node->as_list_expr.items, parse_expr(parser), ASTNode*);
        } else {
            break;
        }
    }

    Token brac = READ_TOKEN(parser);
    if (!consume(parser, RBRAC)) {
        // report error and free node memory

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            brac.index, brac.value.size + brac.index,
            brac.pos, "missing ']' symbol"}),
            Error
            );

        free_ast_node(node);


        return NULL;
    }

    return node;

}

ASTNode* parse_tuple(Parser* parser) {

    consume(parser, LPAREN);
    ASTNode* expr = parse_expr(parser);
    if (READ_TOKEN(parser).type == RPAREN) {
        consume(parser, RPAREN);
        return expr;
    }

    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->as_list_expr.items = (Vector) {INIT_VECTOR_CAP, 0, NULL};
    ALLOCATE(node->as_list_expr.items.arr, ASTNode*, INIT_VECTOR_CAP);
    APPEND(node->as_list_expr.items, expr, ASTNode*);
    node->type = TUPLE_EXPR;

    while(true) {
        if (consume(parser, COMMA)) {
            APPEND(node->as_list_expr.items, parse_expr(parser), ASTNode*);
        } else {
            break;
        }
    }

    Token brac = READ_TOKEN(parser);
    if (!consume(parser, RPAREN)) {
        // report error and free node memory

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            brac.index, brac.value.size + brac.index,
            brac.pos, "missing ')' symbol"}),
            Error
            );

        free_ast_node(node);


        return NULL;
    }

    return node;

}


ASTNode* parse_func_call(Parser* parser) {
    ASTNode* id = parse_identifier_literal(parser);

    Token paren = READ_TOKEN(parser);
    if (!consume(parser, LPAREN)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            paren.index, paren.value.size + paren.index,
            paren.pos, "missing '(' symbol"}),
            Error
            );
        return NULL;
    }

    ASTNode* node = NULL;
    ALLOCATE(node, ASTNode, 1);
    Vector* vec = &(node->as_func_call.params);
    INIT_VECTOR(vec, ASTNode*);
    APPEND(node->as_func_call.params, parse_expr(parser), ASTNode*);
    node->type = FUNC_CALL_EXPR;

    while(true) {
        Token tok = READ_TOKEN(parser);
        if (consume(parser, COMMA)) {
            APPEND(node->as_func_call.params, parse_expr(parser), ASTNode*);
        } else {
            break;
        }
    }

    paren = READ_TOKEN(parser);
    if (!consume(parser, RPAREN)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            paren.index, paren.value.size + paren.index,
            paren.pos, "missing ')' symbol"}),
            Error
            );
        free_ast_node(node);
        return NULL;
    }

    return node;

}

ASTNode* parse_list_index(Parser* parser) {
    ASTNode* id = parse_identifier_literal(parser);
    Token tok = READ_TOKEN(parser);
    if (!consume(parser, LBRAC)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            tok.index, tok.value.size + tok.index,
            tok.pos, "missing ')' symbol"}),
            Error
            );
        free_ast_node(id);
        return NULL;
    }
    ASTNode* expr = parse_expr(parser);

    if (!consume(parser, RBRAC)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            tok.index, tok.value.size + tok.index,
            tok.pos, "missing ')' symbol"}),
            Error
            );
        free_ast_node(id);
        free_ast_node(expr);
        return NULL;
    }
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->type = LIST_INDEX_EXPR;
    node->as_list_index_expr.index = expr;
    node->as_list_index_expr.list_id = id;

    return node;
}

ASTNode* parse_primary(Parser* parser) {
    switch(READ_TOKEN(parser).type) {
        case IDENTIFIER:
            if (PEEK_NEXT(parser).type == LPAREN) {
                return parse_func_call(parser);
            } else if (PEEK_NEXT(parser).type == LBRAC) {
                return parse_list_index(parser);
            }
            return parse_identifier_literal(parser);
        case INT_LIT: return parse_int_literal(parser);
        case LPAREN: return parse_tuple(parser);
        case LBRAC: return parse_list(parser);
        case LCURLY: return parse_block(parser);

        default:
            printf("primary type not implemented: %d\n", READ_TOKEN(parser).type);
            exit(EXIT_FAILURE);
            
    }
    return NULL;

}

ASTNode* parse_block(Parser* parser) {
    consume(parser, LCURLY);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    Vector* vec = &(node->as_compound_statements);
    INIT_VECTOR(vec, ASTNode*);
    node->type = BLOCK_EXPR;

    while(!consume(parser, RCURLY)) {
        APPEND(node->as_compound_statements, parse_statement(parser), ASTNode*);
    }

    return node;

}

ASTNode* parse_elif(Parser* parser) {
    consume(parser, ELIF);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    Vector* vec = &(node->as_if_expr.else_ifs);
    INIT_VECTOR(vec, ASTNode*);
    node->as_if_expr.cond = parse_expr(parser);
    node->as_if_expr.block = parse_block(parser);
    return node;
}

ASTNode* parse_if(Parser* parser) {
    consume(parser, IF);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    Vector* vec = &(node->as_if_expr.else_ifs);
    INIT_VECTOR(vec, ASTNode*);
    node->as_if_expr.cond = parse_expr(parser);
    node->as_if_expr.block = parse_block(parser);
    node->as_if_expr.else_block = NULL;
    while(READ_TOKEN(parser).type == ELIF) {
        APPEND(node->as_if_expr.else_ifs, parse_elif(parser), ASTNode*);
    }

    if (consume(parser, ELSE)) {
        node->as_if_expr.else_block = parse_block(parser);
    } 
    return node;
}

ASTNode* parse_expr(Parser* parser) {
    return parse_or(parser);
}

ASTNode* parse_assginment(Parser* parser) {
    ASTNode* left = parse_expr(parser);
    Token eq = READ_TOKEN(parser);
    if (!consume(parser, EQ)) {
        return left;
    }
    ASTNode* right = parse_expr(parser);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->as_assignment_stmt.left = left;
    node->as_assignment_stmt.right = right;
    node->type = ASSIGNMENT_STMT;

    return node;
}

ASTNode* parse_statement(Parser* parser) {
    ASTNode* node = parse_assginment(parser);
    Token semi_colon = READ_TOKEN(parser);
    if (!consume(parser, SEMICOLON)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            semi_colon.index, semi_colon.value.size + semi_colon.index,
            semi_colon.pos, "missing ';' symbol"}),
            Error
            );
        free_ast_node(node);
        return NULL;
    }
    return node;
}

ASTNode* parse_compound(Parser* parser) {
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    Vector* vec = &(node->as_compound_statements);
    INIT_VECTOR(vec, ASTNode);
    node->type = COMPOUND_STMT;
    while(!consume(parser, TOK_EOF)) {
        APPEND(node->as_compound_statements, parse_statement(parser), ASTNode*);
    }
    return node;
}


int main(int argc, char** argv) {
    Parser* parser;
    ALLOCATE(parser, Parser, 1);
    init_parser(parser);
    load_file_into_memory(&(parser->lexer), "ctest.txt");
    tokenize(&(parser->lexer));
    ASTNode* root = parse_compound(parser);
    print_node(root, 0);

    return 0;
}
