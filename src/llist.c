#include "../include/llist.h"
#include <stdlib.h>

void append_param(Param* list, ASTNode* p) {
    if (list == NULL) return;

    Param* cur = list;
    while (cur->next != NULL) {
        cur = cur->next;
    }
    ALLOC_PARAM(cur->next);
    cur->next->lit = p;
    cur->next->next = NULL;

}

void append_arg(Arg* list, ASTNode* e) {
    if (list == NULL) return;

    Arg* cur = list;
    while (cur->next != NULL) {
        cur = cur->next;
    }
    ALLOC_PARAM(cur->next);
    cur->next->expr = e;
    cur->next->next = NULL;

}