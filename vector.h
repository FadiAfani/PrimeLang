#ifndef VECTOR_H
#define VECTOR_H

#include "memory.h"
#include <stdlib.h>

#define INIT_VECTOR_CAP 16

#define APPEND(vector, elem, type) ({ \
    if (vector->size >= vector->capacity) { \
        REALLOCATE(vector->arr, vector->capacity, type); \
        vector->capacity *= 2; \
    } \
    ((type*) vector->arr)[vector->size++] = elem; \
})

#define INIT_VECTOR(vector, type) ({ \
    ALLOCATE(vector, Vector, 1); \
    ALLOCATE(vector->arr, type, INIT_VECTOR_CAP); \
    vector->capacity = INIT_VECTOR_CAP; \
    vector->size = 0; \
})

typedef struct Vector{
    int capacity;
    int size;
    void* arr;
}Vector;

#endif
