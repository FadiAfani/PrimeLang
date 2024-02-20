#include "vector.h"
#include "memory.h"

#define INIT_VECTOR_CAP 16

Vector* init_vector() {
    Vector* vec;
    ALLOCATE(vec, void, INIT_VECTOR_CAP);
    vec->capacity = 0;
    vec->size = 0;
    vec->arr = NULL;
    return vec;
}

