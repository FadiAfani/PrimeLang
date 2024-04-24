#include "../include/hash_table.h"
#include "../include/memory.h"
#include <stdlib.h>
#include <string.h>


static inline int cmp_values(void* v1, void* v2, int s1, int s2) {
    if (s1 == s2) { 
        return memcmp(v1, v2, s1); 
    } 

    return -1;
}

static inline float get_load_factor(HashTable* ht) {
    return (float) ht->size / ht->capacity;
}

void rehash(HashTable* ht) {
    HashTable new_ht;
    new_ht.size = 0;
    new_ht.capacity = ht->capacity * SCALE_FACTOR;
    ALLOCATE(new_ht.arr, Entry* , ht->capacity * SCALE_FACTOR);
    for (size_t i = 0; i < new_ht.capacity; i++) {
        new_ht.arr[i] = NULL;
    }
    for (size_t i = 0; i < ht->capacity; i++) {
        Entry* e = ht->arr[i];
        if (e != NULL) {
            new_ht.arr[e->key % new_ht.capacity] = e;
            new_ht.size++;
        }
    }
    memcpy(ht, &new_ht, sizeof(HashTable));
}

void init_hash_table(HashTable* ht) {
    ht->size = 0;
    ht->capacity = INIT_TABLE_SIZE;
    ALLOCATE(ht->arr, Entry*, INIT_TABLE_SIZE);
    for (size_t i = 0; i < INIT_TABLE_SIZE; i++) {
        ht->arr[i] = NULL;
    }
}


uint64_t hash(void* value, int size) {
    uint8_t* bytes = value;
    uint64_t h = FNV_OFFSET_BASIS;
    for (int i = 0; i < size; i++) {
        h = h ^ bytes[i];
        h = h * FNV_PRIME;
    }
    return h;
}

void insert(HashTable* ht, void* key, void* value, int size) {
    if (get_load_factor(ht) >= LOAD_FACTOR) {
        REALLOCATE(ht->arr, ht->capacity, Entry*);
        ht->capacity *= SCALE_FACTOR;
        rehash(ht);
    }
    uint64_t h = hash(key, size);
    uint64_t j = h % ht->capacity;
    uint64_t idx = j;
    while(ht->arr[idx] != NULL) {
        idx = (++j) % ht->capacity;
    }
    Entry* e;
    ALLOC_ENTRY(e);
    e->key = h;
    e->size = size;
    e->value = value;
    ht->arr[idx] = e;
    ht->size++;
}

void* lookup(HashTable* ht, void* key, int size) {
    uint64_t h = hash(key, size);
    uint64_t j = h % ht->capacity;
    uint64_t fst = j;
    uint64_t idx = j;

    while(ht->arr[idx] != NULL) {
        Entry* e = ht->arr[idx];
        if (e->key == h) {
            return e->value;
        }
        idx = (++j) % ht->capacity;
        if (idx == fst) return NULL;
    }
    return NULL;
}
