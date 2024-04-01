#include "../include/parser.h"
#include "../include/memory.h"
#include "../include/symbol_table.h"
#include "../include/vector.h"
#include "../include/error.h"
#include "../include/type.h"
#include "../include/scope.h"
#include <stdbool.h>

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
    parser->scopes.sp = -1;
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
            return;
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
            for(int i = 0; i < depth + 1; i++) {
                printf("\t");
            }
            //print_symbol(node->as_type_decl.symbol);
            break;

        case FUNC_DECL:
            printf("<func-decl>\n");
            Vector vec = node->as_func_decl.parameters;
            for(size_t i = 0; i < vec.size; i++) {
                //print_node((INDEX_VECTOR(vec, ASTNode*, i)), depth + 1);
            }
            print_node(node->as_func_decl.block, depth + 1);
            break;
        case FUNC_CALL_EXPR:
            printf("<func-call>\n");
            print_token(*node->as_func_call.func_id);
            for (size_t i = 0; i < node->as_func_call.params.size; i++) {
                print_node(INDEX_VECTOR(node->as_func_call.params, ASTNode*, i), depth + 1);
            }

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
    Token* fst_type = parse_type(parser);
    if (fst_type == NULL) {
        APPEND(
            parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            node->start_tok->index - node->start_tok->pos.col + 1, 
            node->end_tok->pos, 
            node->end_tok->value.size,
            "expected a type" }),
            Error
        );
        return;

    }
    Vector* vec;
    ALLOCATE(vec, Vector, 1);
    INIT_VECTOR((*vec), ASTNode*);
    APPEND((*vec), fst_type, Token*);

    while(consume(parser, ARROW)) {
        APPEND((*vec), parse_type(parser), Token*);
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

    Token* colon = READ_TOKEN(parser);
    if (consume(parser, COLON)) {
        node->end_tok = colon;
        parse_type_annotation(parser, node);
    }

    // get last token from the type vector
    if (node->as_id_literal.id_type != NULL) {
        node->end_tok = INDEX_VECTOR((*node->as_id_literal.id_type), ASTNode*, node->as_id_literal.id_type->size - 1)->end_tok;
    } else {
        node->end_tok = tok;
    }

    Symbol* sym;
    ALLOC_SYMBOL(sym);
    sym->type = SYMBOL_VARIABLE;
    sym->outer_index = -1;
    sym->local_index = -1;
    insert_top(&parser->scopes, (char*) tok->value.arr, sym);

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
        case BOOL_LIT: break;
        case TOK_EOF: break;
        default: 
            parser->cursor++;
            APPEND(parser->parsing_errors,
                ((Error) {SYNTAX_ERROR,
                tok->index - tok->pos.col + 1, 
                tok->pos, 
                tok->value.size, 
                "unexpected symbol"}),
                Error
            );
            break;
    }
    return NULL;
}

ASTNode* parse_unary_expr(Parser* parser) {
    Token* op = READ_TOKEN(parser);
    if (!consume(parser, MINUS)) return NULL;
    ASTNode* expr = parse_expr(parser);
    if (expr == NULL) return NULL;
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->type = UNARY_EXPR;
    node->as_un_expr.op = op;
    node->as_un_expr.operand = expr;
    return node;
    
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
                    node->start_tok->pos, 
                    tok->index - node->start_tok->index + 1,
                    tok->type == MULT ? "expected an expression after '*'" :"expected an expression after '/'" }),
                    Error
                );

                /**
                 * free and return
                 * freeing new_node should also free its child nodes
                */
                free_ast_node(new_node);
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
                    node->start_tok->pos, 
                    tok->index - node->start_tok->index + 1,
                    tok->type == PLUS ? "expected an expression after '+'" :"expected an expression after '-'" }),
                    Error
                );

                // free and return
                free_ast_node(new_node);
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
                    node->start_tok->pos, 
                    tok->index - node->start_tok->index + 1,
                    "expected an expression after '&&'"}),
                    Error
                );
                // free and return
                free_ast_node(new_node);
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
                    node->start_tok->pos, 
                    tok->index - node->start_tok->index + 1,
                    "expected an expression after '||'"}),
                    Error
                );
                // free and return
                free_ast_node(new_node);
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
            ASTNode* item = parse_expr(parser);
            if (item != NULL) {
                APPEND(node->as_list_expr.items, parse_expr(parser), ASTNode*);
            } else {
                APPEND(parser->parsing_errors,
                    ((Error) {SYNTAX_ERROR,
                    tok->index - tok->pos.col + 1, 
                    tok->pos, 
                    tok->value.size,
                    "expected an expression after ','"}),
                    Error
                    );

                // free node and return
                free_ast_node(node);
                return NULL;
            }
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
            node->start_tok->index - node->start_tok->pos.col + 1, 
            tok->pos, 
            node->start_tok->value.size,
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
            brac->index, 
            brac->pos, 
            brac->value.size, "missing ')' symbol"}),
            Error
            );

        free_ast_node(node);


        return NULL;
    }

    return node;

}


ASTNode* parse_func_call(Parser* parser) {
    Token* id_tok = READ_TOKEN(parser);
    consume(parser, IDENTIFIER);

    Token* lparen = READ_TOKEN(parser);
    if (!consume(parser, LPAREN)) return NULL;

    ASTNode* node = NULL;
    ALLOCATE(node, ASTNode, 1);
    node->start_tok = id_tok;
    node->end_tok = node->start_tok;
    node->type = FUNC_CALL_EXPR;
    node->as_func_call.func_id = id_tok;
    INIT_VECTOR(node->as_func_call.params, ASTNode*);
    ASTNode* arg = parse_expr(parser);
    if (arg == NULL) {
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            node->start_tok->index - node->start_tok->pos.col + 1, 
            lparen->pos, 
            lparen->value.size, 
            "expected an expression"}),
            Error
            );
        free_ast_node(node);
        return NULL;
    }
    APPEND(node->as_func_call.params, arg, ASTNode*);

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        Token* tok = READ_TOKEN(parser);
        if (consume(parser, COMMA)) {
            arg = parse_expr(parser);
            if (arg == NULL) {
                APPEND(parser->parsing_errors,
                    ((Error) {SYNTAX_ERROR,
                    node->start_tok->index - node->start_tok->pos.col + 1, 
                    tok->pos, 
                    tok->value.size, 
                    "expected an expression after ','"}),
                    Error
                );
                free_ast_node(node);
                return NULL;
            }
            APPEND(node->as_func_call.params, arg, ASTNode*);
        } else {
            break;
        }
    }

    Token* rparen = READ_TOKEN(parser);
    if (!consume(parser, RPAREN)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            node->start_tok->index - node->start_tok->pos.col + 1, 
            lparen->pos, 
            lparen->value.size, 
            "unclosed delimiter, expected a ')' at the end of the function call"}),
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
            tok->index, 
            tok->pos, 
            tok->value.size, 
            "missing ')' symbol"}),
            Error
            );
        free_ast_node(id);
        return NULL;
    }
    ASTNode* expr = parse_expr(parser);

    if (!consume(parser, RBRAC)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            tok->index, 
            tok->pos, 
            tok->value.size, 
            "missing ')' symbol"}),
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
        case IF: return parse_if(parser);
        case BLOCK_EXPR: return parse_block(parser);
        case BREAK: return parse_break_expr(parser);
        case TOK_EOF: break;

        default: 
            parser->cursor++;
            APPEND(parser->parsing_errors,
                ((Error) {SYNTAX_ERROR,
                tok->index - tok->pos.col + 1, 
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
    node->start_tok = lcurly;
    INIT_SYMBOL_TABLE(node->as_block_expr.table);
    push_scope(&parser->scopes, &node->as_block_expr.table);
    bool unclosed = true;

    while(READ_TOKEN(parser)->type != TOK_EOF) {
        Token* rcurly = READ_TOKEN(parser);
        if (consume(parser, RCURLY)) {
            node->end_tok = rcurly;
            unclosed = false;
            break;
        }
        ASTNode* stmt = parse_statement(parser);
        if (stmt == NULL) return NULL;
        APPEND(node->as_compound_statements, stmt, ASTNode*);
        
    }

    Token* eof_tok = READ_TOKEN(parser);
    if (unclosed && !consume(parser, RCURLY)) {
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            lcurly->index - lcurly->pos.col + 1, 
            lcurly->pos, 
            lcurly->value.size, 
            "unclosed delimiter, missing '}' after a block expression"}),
            Error
            );
        free_ast_node(node);
        return NULL;
    }


    return node;

}

ASTNode* parse_elif(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    consume(parser, ELIF);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    INIT_VECTOR(node->as_if_expr.else_ifs, ASTNode*);
    node->as_if_expr.cond = parse_expr(parser);
    node->start_tok = tok;
    node->end_tok = tok;
    node->type = IF_EXPR;
    if (node->as_if_expr.cond == NULL) {

        Position err_pos = {tok->pos.row, tok->pos.col + 3}; // point to the end of 'elif'
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            node->start_tok->index - node->start_tok->pos.col + 1, 
            err_pos, 
            0, "expected an expression after 'elif'"}),
            Error
        );
        free_ast_node(node);
        return NULL;
    }

    tok = READ_TOKEN(parser);
    if (!consume(parser, THEN)) {

        // tok->value.size - 1 throws away the 0 character
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            node->start_tok->index - node->start_tok->pos.col + 1, 
            node->as_if_expr.cond->end_tok->pos, 
            node->as_if_expr.cond->end_tok->value.size - 1, "expected 'then' after elif condition"}),
            Error
        );
        free_ast_node(node);
        return NULL;
    }
    node->as_if_expr.expr= parse_expr(parser);
    return node;
}

ASTNode* parse_if(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    consume(parser, IF);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    INIT_VECTOR(node->as_if_expr.else_ifs, ASTNode*);
    node->as_if_expr.cond = parse_expr(parser);
    node->start_tok = tok;
    node->end_tok = tok;
    node->type = IF_EXPR;
    if (node->as_if_expr.cond == NULL) {
        Position err_pos = {tok->pos.row, tok->pos.col + 1}; // point to the end of 'if'
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            node->start_tok->index - node->start_tok->pos.col + 1, 
            err_pos, 
            0, "expected an expression after 'if'"}),
            Error
        );
        free_ast_node(node);
        return NULL;
    }

    tok = READ_TOKEN(parser);
    if (!consume(parser, THEN)) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            tok->index - tok->pos.col + 1, 
            tok->pos, 
            tok->value.size - 1, 
            "expected 'then' after an if condition"}),
            Error
            );

        free_ast_node(node);
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

    if (node->as_if_expr.else_expr != NULL) {
        node->end_tok = node->as_if_expr.else_expr->end_tok;
    } else if (node->as_if_expr.else_ifs.size > 0) {
        ASTNode* last_elif = INDEX_VECTOR(node->as_if_expr.else_ifs, ASTNode*, node->as_if_expr.else_ifs.size - 1);
        if (last_elif != NULL) {
            node->end_tok = last_elif->end_tok;
        }
    }

    return node;
}


ASTNode* parse_range(Parser* parser) {
    ASTNode* left = parse_or(parser);
    if (left == NULL) return NULL;
    Token* tok = READ_TOKEN(parser);
    if (!consume(parser, RANGE)) return left;
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->as_bin_expr.left = left;
    node->as_bin_expr.right = parse_or(parser);
    node->type = BIN_EXPR;
    node->as_bin_expr.op = tok;
    node->start_tok = node->as_bin_expr.left->start_tok;
    node->end_tok = node->start_tok;

    if (node->as_bin_expr.right == NULL) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            left->start_tok->index - left->start_tok->pos.col + 1, 
            tok->pos, 
            tok->value.size, 
            "expected an expression after '..'"}),
            Error
        );

        free_ast_node(node);
        return NULL;
    }

    node->end_tok = node->as_bin_expr.right->end_tok;
    return node;
}

ASTNode* parse_comparison(Parser* parser) {
    ASTNode* left = parse_range(parser);
    if (left == NULL) return NULL;

    Token* tok = READ_TOKEN(parser);
    if (!consume(parser, DEQ) 
        && !consume(parser, NEQ) 
        && !consume(parser, BTE) 
        && !consume(parser, BT) 
        && !consume(parser, LTE) 
        && !consume(parser, LT)
        ) return left;

    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->as_bin_expr.left = left;
    node->as_bin_expr.op = tok;
    node->as_bin_expr.right = parse_range(parser);
    node->type= BIN_EXPR;
    node->start_tok = node->as_bin_expr.left->start_tok;

    if (node->as_bin_expr.right == NULL) {

        char* err_msg;
        switch(tok->type) {
            case DEQ:
                err_msg = "expected an expression after '=='";
                break;
            case NEQ:
                err_msg = "expected an expression after '!='";
                break;
            case BTE:
                err_msg = "expected an expression after '>='";
                break;
            case BT:
                err_msg = "expected an expression after '>'";
                break;
            case LTE:
                err_msg = "expected an expression after '<='";
                break;
            case LT:
                err_msg = "expected an expression after '<'";
                break;
            default:
                err_msg = "unexpected symbol";
                break;

        }

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            left->start_tok->index - left->start_tok->pos.col + 1, 
            tok->pos, 
            tok->value.size, 
            err_msg}),
            Error
        );

        free_ast_node(node);
        node->end_tok = node->start_tok;
        return NULL;
    }

    node->end_tok = node->as_bin_expr.right->end_tok;
    return node;
}

Token* parse_type(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    switch (tok->type) {
        case TYPE_INT:
        case TYPE_DOUBLE:
        case TYPE_STRING:
        case TYPE_BOOL:
        case UNIT:
        case IDENTIFIER:
            parser->cursor++;
            break;
        case TOK_EOF: return NULL;
        default:

            APPEND(parser->parsing_errors,
                ((Error) {SYNTAX_ERROR,
                tok->index - tok->pos.col + 1, 
                tok->pos, 
                tok->value.size, 
                "unexpected symbol"}),
                Error
            );
            parser->cursor++;
            return NULL;
    }
    return tok;
}

void parse_type_constructor(Parser* parser, ASTNode* node) {
    Token* const_id = READ_TOKEN(parser);
    if (const_id == NULL) return;
    
    APPEND(node->as_type_decl.enums, const_id, Token*);
    if (!consume(parser, LPAREN)) {
        // no inner types / sub-types to parse
        // append a null representing no inner types
        APPEND(node->as_type_decl.inner_types, NULL, Token*);
        return;
    }
    Token* fst_type = parse_type(parser);
    if (fst_type == NULL) {
        Token* tok = READ_TOKEN(parser);
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            tok->index - tok->pos.col + 1, 
            tok->pos, 
            tok->value.size, 
            "unexpected symbol"}),
            Error
        );
        return;
    }
    Vector* inner_types;
    ALLOCATE(inner_types, ASTNode, 1);
    INIT_VECTOR((*inner_types), ASTNode);
    APPEND((*inner_types), fst_type, Token*);
    while (consume(parser, COMMA)) {
        Token* tok = READ_TOKEN(parser);
        APPEND((*inner_types), parse_type(parser), Token*);
    }

    if (!consume(parser, RPAREN)) {
        //report error and free memory
        Token* tok = READ_TOKEN(parser);
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            tok->index - tok->pos.col + 1, 
            tok->pos, 
            tok->value.size, 
            "expected a ')' after an enum"}),
            Error
        );

        free_ast_node(node);
        return;
    }

    APPEND(node->as_type_decl.inner_types, inner_types, Vector*);
}

ASTNode* parse_type_decl(Parser* parser) {
    consume(parser, KEYWORD_TYPE);
    Token* type_id = READ_TOKEN(parser);
    if (!consume(parser, IDENTIFIER)) return NULL;

    if (!consume(parser, EQ)) {
        Token* tok = READ_TOKEN(parser);
        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            tok->index - tok->pos.col + 1, 
            tok->pos, 
            tok->value.size, 
            "expected a '=' after a type identifier"}),
            Error
        );
        return NULL;
    }

    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->type = TYPE_DECL;
    node->as_type_decl.sym_id = type_id;
    INIT_VECTOR(node->as_type_decl.enums, Token);
    INIT_VECTOR(node->as_type_decl.inner_types, Vector);

    parse_type_constructor(parser, node);
    while (consume(parser, PIPE)) {
        parse_type_constructor(parser, node);
    }

    return node;

}

void parse_func_param(Parser* parser, ASTNode* node) {
    Token* tok = READ_TOKEN(parser);
    if (consume(parser, LPAREN)) {
        APPEND(node->as_func_decl.parameters, parse_identifier_literal(parser), ASTNode*);
        if (!consume(parser, RPAREN)) {

            APPEND(parser->parsing_errors,
                ((Error) {SYNTAX_ERROR,
                node->start_tok->index - node->start_tok->pos.col + 1, 
                tok->pos, 
                tok->value.size, 
                "a parameter should be container within parentheses '(...)'"}),
                Error
            );
            return;
        }
    } else if (consume(parser, UNIT)) {
        APPEND(node->as_func_decl.parameters, parse_type(parser), Token*);
    } else {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            node->start_tok->index - node->start_tok->pos.col + 1, 
            tok->pos, 
            tok->value.size, 
            "a parameter should be container within parentheses '(...)'"}),
            Error
        );
        parser->cursor++;
        return;
    }
}

ASTNode* parse_func_decl(Parser* parser) {
    Token* func_id = READ_TOKEN(parser);
    if (!consume(parser, IDENTIFIER)) return NULL;
    if (!consume(parser, DOUBLE_COLON)) return NULL;

    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    INIT_VECTOR(node->as_func_decl.parameters, ASTNode*);
    node->as_func_decl.sym_id = func_id;
    node->type = FUNC_DECL;
    node->start_tok = func_id;
    node->end_tok = node->start_tok; // set end_tok to start_tok before parsing params

    parse_func_param(parser, node);
    while(consume(parser, ARROW)) {
        parse_func_param(parser, node);
    }

    node->as_func_decl.block = parse_block(parser);
    if (node->as_func_decl.block != NULL) {
        node->end_tok = node->as_func_decl.block->start_tok;
    }

    return node;

}


ASTNode* parse_assignment(Parser* parser) {
    ASTNode* left = parse_comparison(parser);
    if (left == NULL) return NULL;
    Token* eq = READ_TOKEN(parser);
    if (!consume(parser, EQ)) {
        return left;
    }
    ASTNode* right = parse_comparison(parser);
    if (right == NULL) {

        APPEND(parser->parsing_errors,
            ((Error) {SYNTAX_ERROR,
            eq->pos.col + 1, 
            eq->pos, 
            eq->value.size, 
            "expected an expression after '='"}),
            Error
        );
        free_ast_node(left);
        return NULL;
    }
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

ASTNode* parse_break_expr(Parser* parser) {
    Token* tok = READ_TOKEN(parser);
    consume(parser, BREAK);
    ASTNode* node;
    ALLOCATE(node, ASTNode, 1);
    node->type = BREAK_EXPR;
    node->as_break_expr = parse_expr(parser);
    node->start_tok = tok;
    if (node->as_break_expr == NULL) {
        node->end_tok = tok;
    } else {
        node->end_tok = node->as_break_expr->end_tok;
    }

    return node;
}


ASTNode* parse_statement(Parser* parser) {
    switch(READ_TOKEN(parser)->type) {
        case KEYWORD_TYPE:
            return parse_type_decl(parser);
        default:
            break;
        
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
// this might not be needed
ASTNode* parse_program(Parser* parser) {
    ASTNode* node = parse_compound(parser);
    node->type = PROGRAM;
}


