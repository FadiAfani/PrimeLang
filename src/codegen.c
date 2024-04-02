#include "../include/codegen.h"
#include "../include/vector.h"
#include "../include/memory.h"
#include "../include/vm.h"
#include  <stdlib.h>
#include <string.h>

#define INSTS_SIZE 4
#define INT_CONST_SIZE (sizeof(int) + 3)
#define DOUBLE_CONST_SIZE (sizeof(double) + 3)


void compile_literal(ASTNode* node, Compiler* compiler) {
    Const* c;
    ALLOC_CONST(c);
    switch(node->as_literal_expr->type) {
        case INT_LIT:
        {
            int num = (int) atof(node->as_literal_expr->value.arr);

            c->type = INT_CONST;
            c->as_int_const = num;
            c->bytes = (Vector) {INT_CONST_SIZE, 0, NULL};
            ALLOCATE(c->bytes.arr, uint8_t, INT_CONST_SIZE);

            insert_const(&compiler->consts, c);
            APPEND(c->bytes, INT_TAG, uint8_t);
            MEMCPY_VECTOR(c->bytes, &c->const_index, sizeof(uint16_t), uint8_t);
            MEMCPY_VECTOR(c->bytes, &num, sizeof(int), uint8_t);
            APPEND(compiler->code, OP_CONST, uint8_t);
            MEMCPY_VECTOR(compiler->code, &c->const_index, sizeof(uint16_t), uint8_t);
            break;
        }
        case DOUBLE_LIT:
        {
            double num = atof(node->as_literal_expr->value.arr);
            c->type = DOUBLE_CONST;
            c->as_int_const = num;
            c->bytes = (Vector) {DOUBLE_CONST_SIZE, 0, NULL};
            ALLOCATE(c->bytes.arr, uint8_t, DOUBLE_CONST_SIZE);

            insert_const(&compiler->consts, c);
            APPEND(c->bytes, DOUBLE_TAG, uint8_t);
            MEMCPY_VECTOR(c->bytes, &c->const_index, sizeof(uint16_t), uint8_t);
            MEMCPY_VECTOR(c->bytes, &num, sizeof(double), uint8_t);
            APPEND(compiler->code, OP_CONST, uint8_t);
            MEMCPY_VECTOR(compiler->code, &c->const_index, sizeof(uint16_t), uint8_t);
            break;
        }
        case STRING_LIT:
        {
            char* str = (char*) node->as_literal_expr->value.arr;

            c->type = STRING_CONST;
            c->as_string_const = str;
            size_t bytes_size = node->as_literal_expr->value.size + 3;
            c->bytes = (Vector) {bytes_size, 0, NULL};
            ALLOCATE(c->bytes.arr, uint8_t, bytes_size);

            insert_const(&compiler->consts, c);
            APPEND(c->bytes, STRING_TAG, uint8_t);
            MEMCPY_VECTOR(c->bytes, &c->const_index, sizeof(uint16_t), uint8_t);
            MEMCPY_VECTOR(c->bytes, ((uint16_t*) node->as_literal_expr->value.size), sizeof(uint16_t), uint8_t);
            MEMCPY_VECTOR(c->bytes, str, node->as_literal_expr->value.size, uint8_t);
            APPEND(compiler->code, OP_CONST, uint8_t);
            MEMCPY_VECTOR(compiler->code, &c->const_index, sizeof(uint16_t), uint8_t);
            break;
        }
        case IDENTIFIER:
        {
            Symbol* sym = lookup_symbol(&compiler->scopes, (char*) node->as_id_literal.id_token->value.arr);
            if (sym->outer_index > 0) {
                APPEND(compiler->code, OP_LOAD_OUTER, uint8_t);
                MEMCPY_VECTOR(compiler->code, &sym->outer_index, sizeof(uint16_t), uint8_t);
            } else {
                APPEND(compiler->code, OP_LOADL, uint8_t);
                MEMCPY_VECTOR(compiler->code, &sym->local_index, sizeof(uint16_t), uint8_t);
            }
            break;

        }
        default: break;
    }
}

//TODO: refactor, insert print as a symbol during the lexing stage
void compile_func_call(ASTNode* node, Compiler* compiler) {
    /* compile args */
    for (size_t i = 0; i < node->as_func_call.params.size; i++) {
        compile_expr(INDEX_VECTOR(node->as_func_call.params, ASTNode*, i), compiler);
    }
    
    if (strcmp((char*) node->as_func_call.func_id->value.arr, "print") == 0) {
        APPEND(compiler->code, OP_CALL_NATIVE, uint8_t);
        APPEND(compiler->code, NATIVE_PRINT, uint8_t);
        APPEND(compiler->code, (uint8_t) node->as_func_call.params.size, uint8_t);
        for (size_t i = 0; i < node->as_func_call.params.size; i++) {
            ASTNode* arg = INDEX_VECTOR(node->as_func_call.params, ASTNode*, i);
            switch(arg->p_type->as_built_in_type) {
                case INT_TYPE:
                    APPEND(compiler->code, NATIVE_PRINT_INT, uint8_t); 
                    break;
                case DOUBLE_TYPE:
                    APPEND(compiler->code, NATIVE_PRINT_DOUBLE, uint8_t); 
                    break;
                case STRING_TYPE:
                    APPEND(compiler->code, NATIVE_PRINT_STRING, uint8_t); 
                    break;
                default:
                    printf("print is not implemented for this type\n");
                    exit(EXIT_FAILURE);
            }
        }
        return;
    }
    Symbol* sym = lookup_symbol(&compiler->scopes, (char*) node->as_func_call.func_id->value.arr);
   
    if (sym->outer_index > 0) {
        APPEND(compiler->code, OP_LOAD_OUTER, uint8_t);
        uint8_t lsbs = 0XFF & sym->outer_index;
        uint8_t msbs = 0xFF & (sym->outer_index >> sizeof(uint8_t));
        APPEND(compiler->code, msbs, uint8_t);
        APPEND(compiler->code, lsbs, uint8_t);
    } else {
        APPEND(compiler->code, OP_LOADL, uint8_t);
        uint8_t lsbs = 0xFF & sym->local_index;
        uint8_t msbs = 0XFF & (sym->local_index >> sizeof(uint8_t));
        APPEND(compiler->code, msbs, uint8_t);
        APPEND(compiler->code, lsbs, uint8_t);
    }
    APPEND(compiler->code, OP_CALL, uint8_t);
}

/** Block Data Section Structure:
 * <tag> 1 byte
 * <locals-size> 2 bytes
 * <instructions-size> 8 bytes
 * <instructions>
*/

void compile_block_expr(ASTNode* node, Compiler* compiler) {
    push_scope(&compiler->scopes, &node->as_block_expr.table);
    for (size_t i = 0; i < node->as_block_expr.statements.size; i++) {
        compile_statement(
                INDEX_VECTOR(node->as_block_expr.statements, ASTNode*, i),
                compiler
        );
    }

}

void compile_assignment(ASTNode* node, Compiler* compiler) {
    switch(node->as_bin_expr.left->type) {
        case LITERAL_EXPR:
        {
            compile_expr(node->as_bin_expr.right, compiler);
            Symbol* sym = lookup_symbol(&compiler->scopes, node->as_bin_expr.left->as_literal_expr->value.arr);
            APPEND(compiler->code, OP_STOREL, uint8_t);
            MEMCPY_VECTOR(compiler->code, &sym->local_index, sizeof(uint16_t), uint8_t);
            break;
        }
        default:
            printf("unrecognized node type\n");
            exit(EXIT_FAILURE);
    }
    // push a unit value 
}

void compile_binary_expr(ASTNode* node, Compiler* compiler) {
    switch (node->p_type->type_kind) {
        case BUILT_IN:
            
            switch(node->p_type->as_built_in_type) {
                case INT_TYPE:
                    compile_expr(node->as_bin_expr.left, compiler);
                    compile_expr(node->as_bin_expr.right, compiler);
                    switch(node->as_bin_expr.op->type) {
                        case MULT:
                            APPEND(compiler->code, OP_MULT_I, uint8_t);
                            break;
                        case PLUS:
                            APPEND(compiler->code, OP_ADD_I, uint8_t);
                            break;
                        case MINUS:
                            APPEND(compiler->code, OP_SUB_I, uint8_t);
                            break;
                        case DIV:
                            APPEND(compiler->code, OP_DIV_I, uint8_t);
                            break;
                        default: 
                            printf("unexpected type: cannot compile\n");
                            break;

                    }
                    break;
                case DOUBLE_TYPE:
                    compile_expr(node->as_bin_expr.left, compiler);
                    compile_expr(node->as_bin_expr.right, compiler);
                    switch(node->as_bin_expr.op->type) {
                        case MULT:
                            APPEND(compiler->code, OP_MULT_F, uint8_t);
                            break;
                        case PLUS:
                            APPEND(compiler->code, OP_ADD_F, uint8_t);
                            break;
                        case MINUS:
                            APPEND(compiler->code, OP_SUB_F, uint8_t);
                            break;
                        case DIV:
                            APPEND(compiler->code, OP_DIV_F, uint8_t);
                            break;
                        default: 
                            printf("unexpected type: cannot compiled\n");
                            break;
                    }
                    break;
                case UNIT_TYPE:
                    compile_assignment(node, compiler);
                    break;
                default:
                    printf("cannot compiled binary expression\n");
                    break;
        }

    } 
}

void compile_expr(ASTNode* node, Compiler* compiler) {
    switch(node->type) {
        case LITERAL_EXPR: 
            compile_literal(node, compiler);
            break;
        case BIN_EXPR: 
            compile_binary_expr(node, compiler);
            break;
        case FUNC_CALL_EXPR: 
            compile_func_call(node, compiler);
            break;
        case BLOCK_EXPR: 
            compile_block_expr(node, compiler);
            break;
        default:
            printf("code generation is not yet implemented for this expression type\n");
            break;
    }
}

/* Compiled Function Struecture
 * <tag> 1 byte
 * <arity> 1 byte
 * <compiled-block> 
 * */

void compile_function(ASTNode* node, Compiler* compiler) {
    Symbol* sym = lookup_symbol(&compiler->scopes, (char*) node->as_func_decl.sym_id->value.arr);
    /* compile the function's block */
    Compiler lc;
    init_compiler(&lc);
    for (size_t i = 0; i < node->as_block_expr.statements.size; i++) {
        compile_statement(
                INDEX_VECTOR(node->as_block_expr.statements, ASTNode*, i),
                &lc
        );
    }
    Const* c;
    ALLOC_CONST(c);
    c->as_string_const = (char*) node->as_func_decl.sym_id->value.arr;
    c->type = FUNC_CONST;
    Vector* ptr = &c->bytes;
    ALLOC_VECTOR(ptr);

    APPEND(c->bytes, FUNC_TAG, uint8_t); // tag
    MEMCPY_VECTOR(c->bytes, &c->const_index, sizeof(uint16_t), uint8_t);
    APPEND(c->bytes, (uint8_t) sym->as_func_symbol.parameters.size, uint8_t); // arity
    size_t insts_size = lc.code.size;
    MEMCPY_VECTOR(c->bytes, &node->as_block_expr.table.locals_count, sizeof(uint16_t), uint8_t); /* append number of locals to the code portion */
    MEMCPY_VECTOR(c->bytes, &insts_size, INSTS_SIZE, uint8_t); /* append size of the block's instruction section to compiler->data */
    MEMCPY_VECTOR(c->bytes, lc.code.arr, lc.code.size, uint8_t); /* append the block's instructions */

    /**re-hash lc-consts into compiler->consts
     * using MEMCPY_VECTOR_INIT will scramble the constants' indices so its not an option
    */

    for (size_t i = 0; i < lc.consts.size; i++) {
        insert_const(&compiler->consts, INDEX_VECTOR(lc.consts, Const*, i));
    }  

    free_compiler(&lc);
    insert_const(&compiler->consts, c);
}


void compile_statement(ASTNode* node, Compiler* compiler) {
    compile_expr(node, compiler);
}

/** main entry point */
void compile_program(ASTNode* node, Compiler* compiler) {
    for (size_t i = 0; i < node->as_compound_statements.size; i++) {
        compile_statement(
            INDEX_VECTOR(node->as_compound_statements, ASTNode*, i),
            compiler
        );
    }
    
}

/* Binary File Structure:
 * <constant-table-size> 2 bytes
 * <code-section-size> 8 bytes
 * <data-bytes> 
 * <code-bytes> 
 * */

void write_compiler_data(char* filename, Compiler* compiler) {
    FILE* out = fopen(filename, "wb");
    if (out == NULL) return;
    uint8_t* data = compiler->data.arr;
    uint8_t* code = compiler->code.arr; 
    uint16_t consts_size = (uint16_t) compiler->consts.size + 1; // +1 for the main entry point
    fwrite(&consts_size, 1, sizeof(consts_size), out);
    for (int i = 0; i < compiler->consts.capacity; i++) {
        Const* c = INDEX_VECTOR(compiler->consts, Const*, i);
        if (c != NULL) {
            fwrite(c->bytes.arr, 1, c->bytes.size, out);
        }
    }
    /* main entry point */
    fputc(FUNC_TAG, out);
    fputc(0, out); // idx byte 1
    fputc(0, out); // idx byte 2
    fputc(0, out); // arity
    fwrite(&pop_scope(&compiler->scopes)->locals_count, 1, 2, out);
    fwrite(&compiler->code.size, 1, INSTS_SIZE, out);
    fwrite(compiler->code.arr, 1, compiler->code.size, out);
    //fwrite(&compiler->data.arr, 1, compiler->data.size, out);
    fclose(out);

}

void init_compiler(Compiler* compiler) {
    INIT_VECTOR(compiler->data, uint8_t);
    INIT_VECTOR(compiler->code, uint8_t);
    INIT_VECTOR(compiler->consts, Const*);
    memset(compiler->consts.arr, 0, compiler->consts.capacity);
    compiler->scopes.sp = -1;
}

void free_compiler(Compiler* compiler) {
    free(compiler->data.arr);
    free(compiler->code.arr);
    free(compiler->consts.arr);
}
