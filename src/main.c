#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <time.h>
#include <string.h>

#include "../include/vm.h"
#include "../include/parser.h"
#include "../include/vector.h"
#include "../include/codegen.h"
#include "../include/error.h"
#include "../include/prime_file_parser.h"
#include "../include/semantics.h"
#include <assert.h>

/* args
 * argv[1] - input file
 * argv[2] - output file
 * */

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printf("USAGE: ./main arg1 arg2\n");
        return 1;
    }
	Parser* parser;
    SymbolTable* global_syms;
    ALLOC_SYMBOL_TABLE(global_syms);
    init_symbol_table(global_syms);
    ALLOCATE(parser, Parser, 1);
    init_parser(parser);
    push_scope(&parser->scopes, global_syms);
    load_file_into_memory(&(parser->lexer), argv[1]);
    tokenize(&(parser->lexer));
    //print_lexer(&parser->lexer);

    ASTNode* root = parse_program(parser);
    if (parser->parsing_errors.size > 0)  {
        for (size_t i = 0; i < parser->parsing_errors.size; i ++) {
            print_error(INDEX_VECTOR(parser->parsing_errors, Error, i), parser->lexer.filename, parser->lexer.src);
        }
        return -1;
    }
    TypeChecker* tc;
    ALLOC_TYPE_CHECKER(tc);
    init_type_checker(tc);
    push_scope(&tc->scopes, global_syms);
    infer_program(tc, root);
   
    //print_node(root, 0);
    Compiler* compiler;
    ALLOC_COMPILER(compiler);
    init_compiler(compiler);
    push_scope(&compiler->scopes, global_syms);
    compile_program(root, compiler);
    write_compiler_data(argv[2], compiler);

	VM* vm = init_VM();
    vm->mem = parse_prime_file(argv[2]);
    uint8_t insts[5] = {OP_CONST, 0, 0, OP_CALL, OP_HALT};
    vm->code = insts;
    run(vm);

	return 0;

}
