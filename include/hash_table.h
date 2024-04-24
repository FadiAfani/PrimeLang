#ifndef HASH_TABLE_H
#define HASH_TABLE_H

#include <stdint.h>

#define LOAD_FACTOR 0.8

#define FNV_OFFSET_BASIS 0xcbf29ce484222325
#define FNV_PRIME 1099511628211
#define ALLOC_ENTRY(ptr) (ALLOCATE(ptr, Entry*, 1))
#define ALLOC_HASH_TABLE(ptr) (ALLOCATE(ptr, HashTable*, 1))
#define INIT_TABLE_SIZE 16

typedef struct Entry {
    uint64_t key;
    void* value;
    int size;
}Entry;

typedef struct HashTable {
    Entry** arr;
    uint16_t size;
    uint16_t capacity;
}HashTable;

uint64_t hash(void* value, int size);
void insert(HashTable* ht, void* key, void* value, int size);
void* lookup(HashTable* ht, void* key, int size);
void init_hash_table(HashTable* ht);
void free_hash_table(HashTable* ht);
void rehash(HashTable* ht);

#endif
