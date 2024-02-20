#include "memory.h"
#include <stdlib.h>

#define append(vector, elem) ({ \
    if (vector->size >= vector->capacity) { \
        REALLOCATE(vector, vector->capacity, void); \
    } else { \
        vector->arr[vector->size++] = elem; \
    } \
})

typedef struct Vector{
    int capacity;
    int size;
    void* arr;
}Vector;

Vector* init_vector();
