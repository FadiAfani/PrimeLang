#include "parser.h"
#include "memory.h"
#include "vector.h"
#include "error.h"
#include <stdbool.h>
#include "malloc.h"

#define READ_TOKEN(parser) (&(INDEX_VECTOR(parser->lexer.tokens, Token, parser->cursor)))
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
            printf("<literal>: %s\n", (char*) node->as_literal_expr->value.arr);
            if (node->as_literal_expr->type == IDENTIFIER && node->as_id_literal.id_type != NULL) {
                for (size_t i = 0; i < node->as_id_literal.id_type->size; i++) {
                    print_node(INDEX_VECTOR((*node->as_id_literal.id_type), ASTNode*, i), depth + 1);
                }
            }
            break;
        case BIN_EXPR:
            printf("<binary-expr>: %s\n", CAST_VECTOR(node->as_bin_expr.op->value, char));
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
            print_node(node->as_if_expr.expr, depth + 1);
            print_node(node->as_if_expr.else_expr, depth + 1);
            for (size_t i = 0; i < node->as_if_expr.else_ifs.size; i++) {
                print_node(INDEX_VECTOR(node->as_if_expr.else_ifs, ASTNode*, i), depth + 1);
            }
            break;

        case TYPE_DECL:
            printf("<type-decl>\n");
            print_node(node->as_type_decl.sym_id, depth + 1);
            for(int i = 0; i < depth + 1; i++) {
                printf("\t");
            }
            //print_symbol(node->as_type_decl.symbol);
            break;

        case FUNC_DECL:
            printf("<func-decl>\n");
            print_node(node->as_func_decl.sym_id, depth + 1);
            Vector vec = node->as_func_decl.parameters;
            for(size_t i = 0; i < vec.size; i++) {
                print_node((INDEX_VECTOR(vec, ASTNode*, i)), depth + 1);
            }
            print_node(node->as_func_decl.block, depth + 1);
            break;

        case PREDEFINED_TYPE:
            printf("<predefined-type>:");
            switch(node->as_type->type) {
                case UNIT:
                    printf("()\n");
                    break;
                case TYPE_BOOL:
                    printf("bool\n");
                    break;
                case TYPE_BYTE:
                    printf("byte\n");
                    break;
                case TYPE_INT:
                    printf("int\n");
                    break;
                case TYPE_DOUBLE:
                    printf("double\n");
                    break;
                case TYPE_CHAR:
                    printf("char\n");
                    break;
                case TYPE_STRING:
                    printf("string\n");
                    break;
                default:
                    printf("%s", (char*) node->as_type->value.arr);
                    break;
            }
            break;

        default:
            printf("print method is not implemented for this node type: %d\n", node->type);
            exit(EXIT_FAILURE);

    }
}

static bool consume(Parser* parser, TokenType token_type) {
    Token* tok = READ_TOKEN(parser);
    if (tok->type == token_type) {
       parser->cursor++;
       return true;
    } 

    return false;
}

void parse_type_annotation(Parser* parser, ASTNode* node) {
    ASTNode* fst_type = parse_type(parser);
    Vector* vec;
    ALLOCATE(vec, Vector, 1);
    INIT_VECTOR((*vec), ASTNode*);
    APPEND((*vec), fst_type, ASTNode*);

    while(consume(parser, ARROW)) {
        APPEND((*vec), parse_type(parser), ASTNode*);
    }

    if (vec->size == 0) {
        free_vector(vec);
        node->as_id_literal.id_type = NULL;
    } else {
        node->as_id_literal.id_type = vec;
    }
}

ASTNode* parse_identifier_literal(Parser* parser) {

    Token* tok = READ_TOKEN(parser);
    consume(parser,IDENTIFIER);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->start_tok = tok;
    node->as_id_literal.id_token = tok;
    node->as_id_literal.id_type = NULL;
    node->type = LITERAL_EXPR;

    if (consume(parser, COLON)) {
        parse_type_annotation(parser, node);
    }

    // get last token from the type vector
    if (node->as_id_literal.id_type != NULL) {
        node->end_tok = INDEX_VECTOR((*node->as_id_literal.id_type), ASTNode*, node->as_id_literal.id_type->size - 1)->end_tok;
    } else {
        node->end_tok = tok;
    }

    return node;
}

ASTNode* parse_int_literal(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    if (consume(parser, INT_LIT)) {
        ASTNode* node;
        ALLOCATE(node, ASTNode, 1);
        node->as_literal_expr = tok;
        node->type = LITERAL_EXPR;
        node->start_tok = tok;
        node->end_tok = tok;
        return node;
    } 
    
    return NULL;
}

ASTNode* parse_double_literal(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    if (consume(parser, DOUBLE_LIT)) {
        ASTNode* node;
        ALLOCATE(node, ASTNode, 1);
        node->as_literal_expr = tok;
        node->type = LITERAL_EXPR;
        node->start_tok = tok;
        node->end_tok = tok;
        return node;
    } 

    return NULL;
}

ASTNode* parse_string_literal(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    if (consume(parser, STRING_LIT)) {
        ASTNode* node;
        ALLOCATE(node, ASTNode, 1);
        node->as_literal_expr = tok;
        node->type = LITERAL_EXPR;
        node->start_tok = tok;
        node->end_tok = tok;
        return node;
    } 

    return NULL;
}

ASTNode* parse_literal(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    switch(tok->type) {
        case IDENTIFIER:
            return parse_identifier_literal(parser);
        case INT_LIT:
            return parse_int_literal(parser);
        case DOUBLE_LIT:
            return parse_double_literal(parser);
        case STRING_LIT: 
            return parse_string_literal(parser);
        case BOOL_LIT:
        case TOK_EOF: return NULL;
        default: 
            parser->cursor++;
            APPEND(parser->parsing_errors,
                ((Error) {SYNTAX_ERROR,
                tok->index - tok->pos.col + 1, 
                tok->value.size + tok->index,
                tok->pos, 
                tok->value.size, 
                "unexpected symbol"}),
                Error
            );
    }
    return NULL;
}

// <factor> := <literal> ( [ * | \ ] <literal> )*
ASTNode* parse_factor(Parser* parser) {
    ASTNode* node = parse_primary(parser);
    if (node == NULL) return NULL;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        Token* tok = READ_TOKEN(parser);
        if (consume(parser, MULT) || consume(parser, DIV)) {
            ASTNode* new_node;
            ALLOCATE(new_node, ASTNode, 1);
            new_node->as_bin_expr.op = tok;
            new_node->as_bin_expr.left = node;
            new_node->as_bin_expr.right = parse_literal(parser);
            new_node->type = BIN_EXPR;

            new_node->start_tok = new_node->as_bin_expr.left->start_tok;
            if (new_node->as_bin_expr.right == NULL) {
                new_node->end_tok = tok;
                APPEND(
                    parser->parsing_errors,
                    ((Error) {SYNTAX_ERROR,
                    node->start_tok->index - node->start_tok->pos.col + 1, 
                    new_node->end_tok->index + new_node->end_tok->value.size,
                    node->start_tok->pos, 
                    tok->index - node->start_tok->index + 1,
                    tok->type == MULT ? "expected an expression after '*'" :"expected an expression after '/'" }),
                    Error
                );

                // free and return
                return NULL;

                    
            } else {
                new_node->end_tok = new_node->as_bin_expr.right->end_tok;
            }

            node = new_node;
        } else {break;}
    }

    return node;

}

ASTNode* parse_term(Parser* parser) {
    ASTNode* node = parse_factor(parser);
    if (node == NULL) return NULL;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        Token* tok = READ_TOKEN(parser);
        if (consume(parser, PLUS) || consume(parser, MINUS)) {
            ASTNode* new_node;
            ALLOCATE(new_node, ASTNode, 1);
            new_node->as_bin_expr.op = tok;
            new_node->as_bin_expr.left = node;
            new_node->as_bin_expr.right = parse_factor(parser);
            new_node->type = BIN_EXPR;

            new_node->start_tok = new_node->as_bin_expr.left->start_tok;
            if (new_node->as_bin_expr.right == NULL) {
                new_node->end_tok = tok;
                APPEND(
                    parser->parsing_errors,
                    ((Error) {SYNTAX_ERROR,
                    node->start_tok->index - node->start_tok->pos.col + 1, 
                    new_node->end_tok->index + new_node->end_tok->value.size,
                    node->start_tok->pos, 
                    tok->index - node->start_tok->index + 1,
                    tok->type == PLUS ? "expected an expression after '+'" :"expected an expression after '-'" }),
                    Error
                );

                // free and return
                return NULL;
                    
            } else {
                new_node->end_tok = new_node->as_bin_expr.right->end_tok;
            }

            node = new_node;
        } else {break;}
    }
    return node;
}

ASTNode* parse_and(Parser* parser) {
    ASTNode* node = parse_term(parser);
    if (node == NULL) return NULL;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        Token* tok = READ_TOKEN(parser);
        if (consume(parser, AND)) {
            ASTNode* new_node;
            ALLOCATE(new_node, ASTNode, 1);
            new_node->as_bin_expr.op = tok;
            new_node->as_bin_expr.left = node;
            new_node->as_bin_expr.right = parse_term(parser);
            new_node->type = BIN_EXPR;

            new_node->start_tok = new_node->as_bin_expr.left->start_tok;
            if (new_node->as_bin_expr.right == NULL) {
                new_node->end_tok = tok;
                APPEND(
                    parser->parsing_errors,
                    ((Error) {SYNTAX_ERROR,
                    node->start_tok->index - node->start_tok->pos.col + 1, 
                    new_node->end_tok->index + new_node->end_tok->value.size,
                    node->start_tok->pos, 
                    tok->index - node->start_tok->index + 1,
                    "expected an expression after '&&'"}),
                    Error
                );
                // free and return
                return NULL;
                    
            } else {
                new_node->end_tok = new_node->as_bin_expr.right->end_tok;
            }

            node = new_node;
        } else {break;}
    }
    return node;
}

ASTNode* parse_or(Parser* parser) {
    ASTNode* node = parse_and(parser);
    if (node == NULL) return NULL;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        Token* tok = READ_TOKEN(parser);
        if (consume(parser, OR)) {
            ASTNode* new_node;
            ALLOCATE(new_node, ASTNode, 1);
            new_node->as_bin_expr.op = tok;
            new_node->as_bin_expr.left = node;
            new_node->as_bin_expr.right = parse_and(parser);
            new_node->type = BIN_EXPR;

            new_node->start_tok = new_node->as_bin_expr.left->start_tok;
            if (new_node->as_bin_expr.right == NULL) {
                new_node->end_tok = tok;
                APPEND(
                    parser->parsing_errors,
                    ((Error) {SYNTAX_ERROR,
                    node->start_tok->index - node->start_tok->pos.col + 1, 
                    new_node->end_tok->index + new_node->end_tok->value.size,
                    node->start_tok->pos, 
                    tok->index - node->start_tok->index + 1,
                    "expected an expression after '||'"}),
                    Error
                );
                // free and return
                return NULL;
                    
            } else {
                new_node->end_tok = new_node->as_bin_expr.right->end_tok;
            }

            node = new_node;
        } else {break;}
    }
    return node;
}

ASTNode* parse_list(Parser* parser) {

    Token* tok = READ_TOKEN(parser);
    consume(parser, LBRAC);

    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->as_list_expr.items = (Vector) {INIT_VECTOR_CAP, 0, NULL};
    ALLOCATE(node->as_list_expr.items.arr, ASTNode*, INIT_VECTOR_CAP);
    APPEND(node->as_list_expr.items, parse_expr(parser), ASTNode*);
    node->type = LIST_EXPR;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        Token* tok = READ_TOKEN(parser);
        if (consume(parser, COMMA)) {
            APPEND(node->as_list_expr.items, parse_expr(parser), ASTNode*);
        } else {
            break;
        }
    }

    // store starting token and ending token

    node->start_tok = tok;
    if (node->as_list_expr.items.size > 0) {
        node->end_tok = INDEX_VECTOR(node->as_list_expr.items, ASTNode*, node->as_list_expr.items.size - 1)->end_tok;
    } else {
        node->end_tok = tok;
    }

    Token* brac = READ_TOKEN(parser);
    if (!consume(parser, RBRAC)) {
        // report error and free node memory

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            node->start_tok->index - node->start_tok->pos.col + 1, node->end_tok->index + node->end_tok->value.size,
            tok->pos, node->start_tok->value.size,
            "unclosed delimiter, missing ']' at the end of the list literal"}),
            Error
            );

        free_ast_node(node);


        return NULL;
    }

    return node;

}
// TODO: add start_tok and end_tok to the node
ASTNode* parse_tuple(Parser* parser) {

    consume(parser, LPAREN);
    ASTNode* expr = parse_expr(parser);
    if (READ_TOKEN(parser)->type == RPAREN) {
        consume(parser, RPAREN);
        return expr;
    }

    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->as_list_expr.items = (Vector) {INIT_VECTOR_CAP, 0, NULL};
    ALLOCATE(node->as_list_expr.items.arr, ASTNode*, INIT_VECTOR_CAP);
    APPEND(node->as_list_expr.items, expr, ASTNode*);
    node->type = TUPLE_EXPR;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        if (consume(parser, COMMA)) {
            APPEND(node->as_list_expr.items, parse_expr(parser), ASTNode*);
        } else {
            break;
        }
    }

    Token* brac = READ_TOKEN(parser);
    if (!consume(parser, RPAREN)) {
        // report error and free node memory

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            brac->index, brac->value.size + brac->index,
            brac->pos, brac->value.size, "missing ')' symbol"}),
            Error
            );

        free_ast_node(node);


        return NULL;
    }

    return node;

}


ASTNode* parse_func_call(Parser* parser) {
    ASTNode* id = parse_identifier_literal(parser);

    Token* paren = READ_TOKEN(parser);
    if (!consume(parser, LPAREN)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            paren->index, paren->value.size + paren->index,
            paren->pos, paren->value.size, "missing '(' symbol"}),
            Error
            );
        return NULL;
    }

    ASTNode* node = NULL;
    ALLOCATE(node, ASTNode, 1);
    INIT_VECTOR(node->as_func_call.params, ASTNode*);
    APPEND(node->as_func_call.params, parse_expr(parser), ASTNode*);
    node->type = FUNC_CALL_EXPR;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        Token* tok = READ_TOKEN(parser);
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
            paren->index, paren->value.size + paren->index,
            paren->pos, paren->value.size, "missing ')' symbol"}),
            Error
            );
        free_ast_node(node);
        return NULL;
    }

    return node;

}

ASTNode* parse_list_index(Parser* parser) {
    ASTNode* id = parse_identifier_literal(parser);
    Token* tok = READ_TOKEN(parser);
    if (!consume(parser, LBRAC)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            tok->index, tok->value.size + tok->index,
            tok->pos, tok->value.size, "missing ')' symbol"}),
            Error
            );
        free_ast_node(id);
        return NULL;
    }
    ASTNode* expr = parse_expr(parser);

    if (!consume(parser, RBRAC)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            tok->index, tok->value.size + tok->index,
            tok->pos, tok->value.size, "missing ')' symbol"}),
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
    Token* tok = READ_TOKEN(parser);
    switch(READ_TOKEN(parser)->type) {
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
        case IF_EXPR: return parse_if(parser);
        case BLOCK_EXPR: return parse_block(parser);
        case TOK_EOF: break;

        default: 
            parser->cursor++;
            APPEND(parser->parsing_errors,
                ((Error) {SYNTAX_ERROR,
                tok->index - tok->pos.col + 1, 
                tok->value.size + tok->index,
                tok->pos, 
                tok->value.size, 
                "unexpected symbol"}),
                Error
            );
            break;
    }
    return NULL;

}


ASTNode* parse_block(Parser* parser) {
    Token* lcurly = READ_TOKEN(parser);
    consume(parser, LCURLY);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    INIT_VECTOR(node->as_compound_statements, ASTNode*);
    node->type = BLOCK_EXPR;
    bool unclosed = true;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        ASTNode* stmt = parse_statement(parser);
        APPEND(node->as_compound_statements, stmt, ASTNode*);
        if (consume(parser, RCURLY)) {
            unclosed = false;
            break;
        }
    }
    Token* eof_tok = READ_TOKEN(parser);
    if (unclosed) {
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            lcurly->index - lcurly->pos.col + 1, 
            lcurly->index + 1,
            lcurly->pos, 
            lcurly->value.size, 
            "unclosed delimiter, missing '}' after a block expression"}),
            Error
            );
        // free node 
    }
    INIT_SYMBOL_TABLE(node->as_block_expr.table);


    return node;

}

ASTNode* parse_elif(Parser* parser) {
    consume(parser, ELIF);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    INIT_VECTOR(node->as_if_expr.else_ifs, ASTNode*);
    node->as_if_expr.cond = parse_expr(parser);
    node->type = IF_EXPR;

    Token* tok = READ_TOKEN(parser);
    if (consume(parser, THEN)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            tok->index, tok->value.size + tok->index,
            tok->pos, tok->value.size, "missing 'then' after if condition"}),
            Error
            );
    }
    node->as_if_expr.expr= parse_expr(parser);
    return node;
}

ASTNode* parse_if(Parser* parser) {
    consume(parser, IF);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    INIT_VECTOR(node->as_if_expr.else_ifs, ASTNode*);
    node->as_if_expr.cond = parse_expr(parser);
    node->type = IF_EXPR;

    Token* tok = READ_TOKEN(parser);
    if (!consume(parser, THEN)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            tok->index - tok->pos.col + 1, 
            tok->value.size + tok->index,
            tok->pos, tok->value.size, 
            "missing 'then' after an if condition"}),
            Error
            );
        //TODO: free node
        return NULL;
    }
    node->as_if_expr.expr = parse_expr(parser);
    node->as_if_expr.else_expr = NULL;
    while(READ_TOKEN(parser)->type == ELIF) {
        APPEND(node->as_if_expr.else_ifs, parse_elif(parser), ASTNode*);
    }

    if (consume(parser, ELSE)) {
        node->as_if_expr.else_expr = parse_expr(parser);
    } 
    return node;
}


ASTNode* parse_range(Parser* parser) {
    ASTNode* left = parse_or(parser);
    Token* tok = READ_TOKEN(parser);
    if (!consume(parser, RANGE)) return left;
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->as_bin_expr.left = left;
    node->as_bin_expr.right = parse_or(parser);
    node->type = BIN_EXPR;
    node->as_bin_expr.op = tok;
    node->start_tok = node->as_bin_expr.left->start_tok;

    if (node->as_bin_expr.right == NULL) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            left->start_tok->index - left->start_tok->pos.col + 1, 
            tok->value.size + tok->index,
            tok->pos, 
            tok->value.size, 
            "expected an expression after '..'"}),
            Error
        );

        //TODO: free node
        node->end_tok = node->start_tok;
        return NULL;
    }

    node->end_tok = node->as_bin_expr.right->end_tok;
    return node;
}

ASTNode* parse_equality(Parser* parser) {
    ASTNode* left = parse_range(parser);

    if (left == NULL) {
        //TODO: free memory
        return NULL;
    }

    Token* tok = READ_TOKEN(parser);
    if (!consume(parser, DEQ)) return left;

    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->as_bin_expr.left = left;
    node->as_bin_expr.op = tok;
    node->as_bin_expr.right = parse_range(parser);
    node->type= BIN_EXPR;
    node->start_tok = node->as_bin_expr.left->start_tok;

    if (node->as_bin_expr.right == NULL) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            left->start_tok->index - left->start_tok->pos.col + 1, 
            tok->value.size + tok->index,
            tok->pos, 
            tok->value.size, 
            "expected an expression after '=='"}),
            Error
        );

        //TODO: free node
        node->end_tok = node->start_tok;
        return NULL;
    }

    node->end_tok = node->as_bin_expr.right->end_tok;
    return node;
}

ASTNode* parse_type(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    ASTNode* node;
    switch (tok->type) {
        case TYPE_INT:
        case TYPE_DOUBLE:
        case TYPE_STRING:
        case TYPE_BOOL:
        case UNIT:
            ALLOCATE(node, ASTNode, 1);
            node->as_type = tok;
            node->type = PREDEFINED_TYPE;
            parser->cursor++;
            break;
        default:
            node = parse_identifier_literal(parser);
            break;
    }
    return node;
}

void parse_type_constructor(Parser* parser, ASTNode* node) {
    ASTNode* const_id = parse_identifier_literal(parser);
    APPEND(node->as_type_decl.enums, const_id, ASTNode*);
    if (!consume(parser, LPAREN)) {
        // no inner types / sub-types to parse
        // append a null representing no inner types
        APPEND(node->as_type_decl.inner_types, NULL, ASTNode*);
        return;
    }
    ASTNode* fst_type = parse_type(parser);
    Vector* inner_types;
    ALLOCATE(inner_types, ASTNode, 1);
    INIT_VECTOR((*inner_types), ASTNode);
    APPEND((*inner_types), fst_type, ASTNode*);
    while (consume(parser, COMMA)) {
        APPEND((*inner_types), parse_type(parser), ASTNode*);
    }
    APPEND(node->as_type_decl.inner_types, inner_types, Vector*);

    if (!consume(parser, RPAREN)) {
        //report error and free memory
        return;
    }
}

ASTNode* parse_type_decl(Parser* parser) {
    consume(parser, KEYWORD_TYPE);
    ASTNode* type_id = parse_identifier_literal(parser);

    if (!consume(parser, EQ)) {
        // report error
        free_ast_node(type_id);
        return NULL;
    }

    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->type = TYPE_DECL;
    node->as_type_decl.sym_id = type_id;
    INIT_VECTOR(node->as_type_decl.enums, Symbol);
    INIT_VECTOR(node->as_type_decl.inner_types, Vector);

    parse_type_constructor(parser, node);
    while (consume(parser, PIPE)) {
        parse_type_constructor(parser, node);
    }

    return node;

}

void parse_func_param(Parser* parser, ASTNode* node) {
    if (READ_TOKEN(parser)->type == LPAREN) {
        consume(parser, LPAREN);
        APPEND(node->as_func_decl.parameters, parse_identifier_literal(parser), ASTNode*);
        if (!consume(parser, RPAREN)) {
            //report error and free memory
            return;
        }
    } else if (READ_TOKEN(parser)->type == UNIT) {
        APPEND(node->as_func_decl.parameters, parse_type(parser), ASTNode*);
    } else {
        // report error and free memory
        return;
    }
}

ASTNode* parse_func_decl(Parser* parser) {
    ASTNode* func_id = parse_identifier_literal(parser);
    if (!consume(parser, DOUBLE_COLON)) {
        // report an error
        free_ast_node(func_id);
        return NULL;
    }
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    INIT_VECTOR(node->as_func_decl.parameters, ASTNode*);
    node->as_func_decl.sym_id = func_id;
    node->type = FUNC_DECL;

    parse_func_param(parser, node);
    while(consume(parser, ARROW)) {
        parse_func_param(parser, node);
    }

    node->as_func_decl.block = parse_block(parser);

    return node;

}

ASTNode* parse_block_based_expr(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    switch(tok->type) {
        case IF: return parse_if(parser);
        case LCURLY: return parse_block(parser);
        case TOK_EOF: break;
        default: return parse_equality(parser);
    }
    return NULL;
}

ASTNode* parse_assignment(Parser* parser) {
    ASTNode* left = parse_block_based_expr(parser);
    if (left == NULL) return NULL;
    Token* eq = READ_TOKEN(parser);
    if (!consume(parser, EQ)) {
        return left;
    }
    ASTNode* right = parse_block_based_expr(parser);
    if (right == NULL) return NULL;
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->as_bin_expr.op = eq;
    node->as_bin_expr.left = left;
    node->as_bin_expr.right = right;
    node->type = BIN_EXPR;

    node->start_tok = node->as_bin_expr.left->start_tok;
    node->end_tok = node->as_bin_expr.right->end_tok;
    return node;
}



ASTNode* parse_expr(Parser* parser) {
    ASTNode* node = parse_assignment(parser);
    return node;
}


ASTNode* parse_statement(Parser* parser) {
    switch(READ_TOKEN(parser)->type) {
        case KEYWORD_TYPE:
            return parse_type_decl(parser);
        
    }
    if (READ_TOKEN(parser)->type == IDENTIFIER && PEEK_NEXT(parser).type == DOUBLE_COLON) {
        return parse_func_decl(parser);
    }
    ASTNode* node = parse_expr(parser);
    return node;
}


ASTNode* parse_compound(Parser* parser) {
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    INIT_VECTOR(node->as_compound_statements, ASTNode);
    node->type = COMPOUND_STMT;
    while(READ_TOKEN(parser)->type != TOK_EOF) {
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
    //print_lexer(&parser->lexer);
    for (size_t i = 0; i < parser->parsing_errors.size; i ++) {
        print_error(INDEX_VECTOR(parser->parsing_errors, Error, i), parser->lexer.filename, parser->lexer.src);
    }
    //print_node(root, 0);

    return 0;
}
