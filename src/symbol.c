#include "../include/symbol.h"

void init_func_symbol(FuncSymbol* fs) {
    INIT_VECTOR(fs->parameters, Symbol*);
    ALLOC_TYPE(fs->func_type);
}
