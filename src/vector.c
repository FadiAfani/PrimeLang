#include "../include/vector.h"
#include "../include/memory.h"

void free_vector(Vector* vec) {
    free(vec->arr);
    free(vec);
}




