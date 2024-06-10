#include "../include/codegen.h"
#include "../include/vector.h"
#include "../include/memory.h"
#include "../include/vm.h"
#include  <stdlib.h>
#include <string.h>
#include <assert.h>

#define INSTS_SIZE 4
#define INT_CONST_SIZE (sizeof(int) + 3)
#define DOUBLE_CONST_SIZE (sizeof(double) + 3)


static void cpy_comp(Compiler* dest, Compiler* src) {
    MEMCPY_VECTOR(dest->code, src->code.arr, src->code.size, uint8_t);
    for (size_t i = 0; i < src->consts.capacity; i++) {
        Entry* e = src->consts.arr[i];
        if (e != NULL) {
            Const* c;
            ALLOC_CONST(c);
            memcpy(c, e->value, sizeof(Const));
            c->const_index = dest->consts.size + 1;
            insert(&dest->consts, &c->as_string_const, c, e->size);
        }
        
    }
    for (int i = 0; i <= src->scopes.sp; i++) {
        push_scope(&dest->scopes, src->scopes.stack[i]);
    }
}

static void cpy_scopes(Compiler* dest, Compiler* src) {
    for (int i = 0; i <= src->scopes.sp; i++) {
        push_scope(&dest->scopes, src->scopes.stack[i]);
    }
}

/* <constant format>
 * 
 * <tag> 1 byte
 * <index> 2 bytes
 * <value> variable
 */

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

            c->const_index = compiler->consts.size + 1;
            insert(&compiler->consts, &num, c, sizeof(int));
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

            c->const_index = compiler->consts.size + 1;
            insert(&compiler->consts, &num, c, sizeof(double));
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

            c->const_index = compiler->consts.size + 1;
            insert(&compiler->consts, str, c, node->as_literal_expr->value.size);
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
            Symbol* sym = lookup_symbol(&compiler->scopes, (char*) node->as_id_literal.id_token->value.arr, node->as_id_literal.id_token->value.size);
            if (sym->outer_index < 0) {
                APPEND(compiler->code, OP_LOADL, uint8_t);
                MEMCPY_VECTOR(compiler->code, &sym->local_index, sizeof(uint16_t), uint8_t);
            } else {
                APPEND(compiler->code, OP_LOADH, uint8_t);
                MEMCPY_VECTOR(compiler->code, &sym->outer_index, sizeof(uint16_t), uint8_t);
            }
            break;

        }
        default: break;
    }
}

//TODO: refactor, insert print as a symbol during the lexing stage
void compile_func_call(ASTNode* node, Compiler* compiler) {

    /* compile args */
    // TODO: fix arg position (not stacked correctly)
    
    if (strcmp((char*) node->as_func_call.func_id->value.arr, "print") == 0) {

        Arg* al = node->as_func_call.arg_list;
        while (al != NULL) {
            compile_expr(al->expr, compiler);
            al = al->next;
        }
    
        APPEND(compiler->code, OP_CALL_NATIVE, uint8_t);
        APPEND(compiler->code, NATIVE_PRINT, uint8_t);
        APPEND(compiler->code, (uint8_t) node->as_func_call.argc, uint8_t);
        al = node->as_func_call.arg_list;
        while (al != NULL) {
            ASTNode* arg = al->expr;
            al = al->next;
            //TODO: handle non-built-ins
            switch(arg->p_type->type_kind) {
                case BUILT_IN:

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
                    break;
                default:
                    printf("unprintable type\n");
                    exit(EXIT_FAILURE);
                }
        }
        return;
    }
    void* key = node->as_func_call.func_id->value.arr;
    int ksize = node->as_func_call.func_id->value.size;
    Symbol* sym = lookup_symbol(&compiler->scopes, (char*) key, ksize);
    Const* c;
    if (sym->as_func_symbol.parent_func == NULL) {

        c = lookup(&compiler->consts, key, ksize);
        Arg* al = node->as_func_call.arg_list;
        while (al != NULL) {
            compile_expr(al->expr, compiler);
            al = al->next;
        }

    } else {

        Token* pf = sym->as_func_symbol.parent_func;
        c = lookup(&compiler->consts, pf->value.arr, pf->value.size);
        Arg* al = sym->as_func_symbol.applied_args;
        while (al != NULL) {
            compile_expr(al->expr, compiler);
            al = al->next;
        }
        al = node->as_func_call.arg_list;
        while (al != NULL) {
            compile_expr(al->expr, compiler);
            al = al->next;
        }
    }


    APPEND(compiler->code, OP_CONST, uint8_t);
    uint8_t lsbs = 0xFF & c->const_index;
    uint8_t msbs = c->const_index >> 8;
    APPEND(compiler->code, lsbs, uint8_t);
    APPEND(compiler->code, msbs, uint8_t);
    APPEND(compiler->code, OP_CALL, uint8_t);


}

/** Block Data Section Structure:
 * <tag> 1 byte
 * <locals-size> 2 bytes
 * <instructions-size> 8 bytes
 * <instructions>
*/

void compile_block_expr(ASTNode* node, Compiler* compiler) {
    printf("stack pointer: %d", compiler->scopes.sp);
    push_scope(&compiler->scopes, &node->as_block_expr.table);
    for (size_t i = 0; i < node->as_block_expr.statements.size; i++) {
        compile_statement(
                INDEX_VECTOR(node->as_block_expr.statements, ASTNode*, i),
                compiler
        );
    }
    pop_scope(&compiler->scopes);

}

void compile_assignment(ASTNode* node, Compiler* compiler) {
    switch(node->as_bin_expr.left->type) {
        case LITERAL_EXPR:
        {
            compile_expr(node->as_bin_expr.right, compiler);
            Symbol* sym = lookup_symbol(&compiler->scopes, node->as_bin_expr.left->as_literal_expr->value.arr, node->as_bin_expr.left->as_literal_expr->value.size);
            if (sym->outer_index < 0) {
                APPEND(compiler->code, OP_STOREL, uint8_t);
                MEMCPY_VECTOR(compiler->code, &sym->local_index, sizeof(uint16_t), uint8_t);
            } else {
                APPEND(compiler->code, OP_STOREH, uint8_t);
                MEMCPY_VECTOR(compiler->code, &sym->outer_index, sizeof(uint16_t), uint8_t);
            }
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
                        case BT:
                            APPEND(compiler->code, OP_CMP_I, uint8_t);
                            APPEND(compiler->code, OP_BT, uint8_t);
                            break;
                        case BTE:
                            APPEND(compiler->code, OP_CMP_I, uint8_t);
                            APPEND(compiler->code, OP_BTE, uint8_t);
                            break;
                        case LT:
                            APPEND(compiler->code, OP_CMP_I, uint8_t);
                            APPEND(compiler->code, OP_LT, uint8_t);
                            break;
                        case LTE:
                            APPEND(compiler->code, OP_CMP_I, uint8_t);
                            APPEND(compiler->code, OP_LTE, uint8_t);
                            break;
                        case EQ:
                            APPEND(compiler->code, OP_CMP_I, uint8_t);
                            APPEND(compiler->code, OP_EQ, uint8_t);
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
                        case BT:
                            APPEND(compiler->code, OP_CMP_F, uint8_t);
                            APPEND(compiler->code, OP_BT, uint8_t);
                            break;
                        case BTE:
                            APPEND(compiler->code, OP_CMP_F, uint8_t);
                            APPEND(compiler->code, OP_BTE, uint8_t);
                            break;
                        case LT:
                            APPEND(compiler->code, OP_CMP_F, uint8_t);
                            APPEND(compiler->code, OP_LT, uint8_t);
                            break;
                        case LTE:
                            APPEND(compiler->code, OP_CMP_F, uint8_t);
                            APPEND(compiler->code, OP_LTE, uint8_t);
                            break;
                        case EQ:
                            APPEND(compiler->code, OP_CMP_F, uint8_t);
                            APPEND(compiler->code, OP_EQ, uint8_t);
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
        case IF_EXPR:
            compile_if_expr(node, compiler);
            break;
        default:
            printf("code generation is not yet implemented for this expression type: %d\n", node->type);
            break;
    }
}

/* Compiled Function Format 
 * <tag>            1 byte
 * <index>          2 bytes
 * <arity>          1 byte
 * <locals>         2 bytes
 * <code-size>      4 bytes
 * <code-section>   <code-size> bytes
 *
 * */

void compile_function(ASTNode* node, Compiler* compiler) {
    /* initialize constant */
    Const* c;
    ALLOC_CONST(c);
    INIT_VECTOR(c->bytes, uint8_t);
    c->as_string_const = (char*) node->as_func_decl.sym_id->value.arr;
    c->type = FUNC_CONST;
    c->const_index = compiler->consts.size + 1;
    insert(&compiler->consts, c->as_string_const, c, node->as_func_decl.sym_id->value.size);

    Symbol* sym = lookup_symbol(&compiler->scopes, (char*) node->as_func_decl.sym_id->value.arr, node->as_func_decl.sym_id->value.size);
    /* compile the function's block */
    Compiler lc;
    init_compiler(&lc);
    cpy_scopes(&lc, compiler);
    push_scope(&lc.scopes, &node->as_func_decl.block->as_block_expr.table);


    lc.consts.size = compiler->consts.size; /* skip the indices already inserted by "compiler"*/
    ASTNode* block = node->as_func_decl.block;
    for (size_t i = 0; i < block->as_block_expr.statements.size; i++) {
        compile_statement(
                INDEX_VECTOR(block->as_block_expr.statements, ASTNode*, i),
                &lc
        );
    }
    APPEND(lc.code, OP_RET, uint8_t);

    /**re-hash lc-consts into compiler->consts
     * using MEMCPY_VECTOR_INIT will scramble the constants' indices so its not an option
    */

    for (size_t i = 0; i < lc.consts.capacity; i++) {
        Entry* lc_const = lc.consts.arr[i];
        if (lc_const != NULL) {
            /* treat the key as a sequence of chars hence the 'as_string_const */
            Const* lcc = lc_const->value;
            lcc->const_index = compiler->consts.size + 1;
            void* key = &lcc->as_string_const;
            insert(&compiler->consts, key, lc_const->value, lc_const->size);
        }
    }  


    APPEND(c->bytes, FUNC_TAG, uint8_t); // tag
    MEMCPY_VECTOR(c->bytes, &c->const_index, sizeof(uint16_t), uint8_t);
    APPEND(c->bytes, (uint8_t) sym->as_func_symbol.pc, uint8_t); // arity
    size_t insts_size = lc.code.size; /* OP_RET + params * (OP_STORE + INDEX) */
    MEMCPY_VECTOR(c->bytes, &node->as_func_decl.block->as_block_expr.table.locals_count, sizeof(uint16_t), uint8_t); /* append number of locals to the code portion */
    MEMCPY_VECTOR(c->bytes, &insts_size, INSTS_SIZE, uint8_t); /* append size of the block's instruction section to compiler->data */
    MEMCPY_VECTOR(c->bytes, lc.code.arr, lc.code.size, uint8_t); /* append the block's instructions */

    free_compiler(&lc);
}

void compile_if_expr(ASTNode* node, Compiler* compiler) {
    size_t es = node->as_if_expr.else_ifs.size;
    size_t jmp_holes[es + 1];
    ASTNode* cur_node;
    for (size_t i = 0; i < es + 1; i++) {
        Compiler lc;
        init_compiler(&lc);
        cpy_scopes(&lc, compiler);
        if (i == 0)
            cur_node = node;
        else
            cur_node = INDEX_VECTOR(node->as_if_expr.else_ifs, ASTNode*, i - 1);

        compile_expr(cur_node->as_if_expr.cond, compiler);
        lc.consts.size = compiler->consts.size; // skip already assigned indices
        compile_expr(cur_node->as_if_expr.expr, &lc);
        APPEND(compiler->code, OP_JMP_REL_FALSE, uint8_t);

        /* account for the relative jump which skips the entire if expression */
        APPEND(compiler->code, (lc.code.size + 3) & 0xFF, uint8_t);
        APPEND(compiler->code, (lc.code.size + 3) >> 8, uint8_t);
        cpy_comp(compiler, &lc);
        jmp_holes[i] = compiler->code.size;
        compiler->code.size += 3; // skip jmp instruction 
        free_compiler(&lc);
    }
    if (node->as_if_expr.else_expr != NULL) {
        compile_expr(node->as_if_expr.else_expr, compiler);
    }

    size_t bottom = compiler->code.size;
    uint8_t* arr = compiler->code.arr;
    for (size_t i = 0; i < es + 1; i++) {
        size_t idx = jmp_holes[i];
        arr[idx] = OP_JMP_ABS;
        arr[idx + 1] = bottom & 0xFF;
        arr[idx + 2] = bottom >> 8;
    }
    
}


void compile_statement(ASTNode* node, Compiler* compiler) {
    switch(node->type) {
        case FUNC_DECL:
            compile_function(node, compiler);
            break;
        default:
            compile_expr(node, compiler);
            break;
    }
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
 * <code-section-size> 4 bytes
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
        Entry* e = compiler->consts.arr[i];
        if (e != NULL) {
            Const* c = e->value;
            fwrite(c->bytes.arr, 1, c->bytes.size, out);
        }
    }
    /* main entry point */
    APPEND(compiler->code, OP_RET, uint8_t); // return at the end of main
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
    init_hash_table(&compiler->consts);
    compiler->scopes.sp = -1;
    compiler->scopes.outers = 0;
}

void free_compiler(Compiler* compiler) {
    free(compiler->data.arr);
    free(compiler->code.arr);
    free(compiler->consts.arr);
}
