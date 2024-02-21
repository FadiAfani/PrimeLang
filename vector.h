#ifndef VECTOR_H
#define VECTOR_H

#include "memory.h"
#include <stdlib.h>

#define APPEND(vector, elem, type) ({ \
    if (vector->size >= vector->capacity) { \
        REALLOCATE(vector, vector->capacity, void); \
        ((type*) vector->arr)[vector->size++] = elem; \
    } else { \
        ((type*) vector->arr)[vector->size++] = elem; \
    } \
})

typedef struct Vector{
    int capacity;
    int size;
    void* arr;
}Vector;

Vector* init_vector();

#endif
