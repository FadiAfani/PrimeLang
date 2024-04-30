#ifndef L_LIST_H
#define L_LIST_H

#include "memory.h"

typedef struct ASTNode ASTNode;
typedef struct Param Param;

#define ALLOC_PARAM(ptr) (ALLOCATE(ptr, Param, 1))
#define ALLOC_ARG(ptr) (ALLOCATE(ptr, Arg, 1))


struct Param {
    ASTNode* lit;
    Param* next;
};

typedef struct Arg Arg;

struct Arg {
    ASTNode* expr;
    Arg* next;
};
void append_param(Param* list, ASTNode* p);
void append_arg(Arg* list, ASTNode* e);


#endif