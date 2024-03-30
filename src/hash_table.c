#include "./hash_table.h"
#include "./memory.h"
#include <stdlib.h>
#include <string.h>

#define CAP_FACTOR 1.3
#define FNV_OB 0xCBF29CE484222325
#define FNV_PRIME 0x100000001b3
#define HASH_THRES 32 /* defines when to use a hash function */

HashTable* init_hash_table(uint16_t num_keys) {
    uint16_t cap = CAP_FACTOR * num_keys + 1;
    HashTableEntry* entries;
    ALLOCATE(entries, HashTableEntry, cap);
    //memset(entries, -1, sizeof(HashTableEntry) * cap);
    for (int i = 0; i < cap; i++) {
        entries[i].key = -1;
        entries[i].val = (Value) {0};
    }
    HashTable* table;
    ALLOCATE(table, HashTable, 1);
    table->capacity = cap;
    table->table = entries;

    return table;
}

uint16_t hash(uint16_t key, uint16_t m) {
    if (m <= 32) return key;
    uint64_t res = FNV_OB;
    // first-byte
    res *= FNV_PRIME;
    res ^= key & 255;
    // second-byte
    res *= FNV_PRIME;
    res ^= (key >> 8) & 255;

    return res % m;
}

/* unsigned ints wrap around 
 * assuming the capacity is always larger than the universe of keys
 * eventually every key will be inserted
 * so infinite loops are safe
 * */
void insert(HashTable* hash_table, uint16_t key, Value val) { 
    uint16_t cap = hash_table->capacity;
    if (cap <= HASH_THRES) {
        hash_table->table[key] = (HashTableEntry) {.key = key, .val = val};
        return;
    }
    uint16_t h = hash(key, hash_table->capacity);
    for (;;) {
        if (hash_table->table[h].key == -1) {
            hash_table->table[h] = (HashTableEntry) {.key = key, val = val};
        } else {h++;}
    }
}

/* relies on the compiler's correctness
 * not sure if this is a good idea
 * might cause infinite loops if a bug were to occur
 * */

Value lookup(HashTable* hash_table, uint16_t key) {
    uint16_t cap = hash_table->capacity;
    if (cap <= HASH_THRES) {
        return hash_table->table[key].val; 
    }
    uint16_t h = hash(key, cap);
    Value val = {0};
    for (;;) {
        HashTableEntry entry = hash_table->table[h & cap];
        if (entry.key == -1) return val; /* entry is not the table */

        if (entry.key == key) {
            val = entry.val;
        } else {h++;}
    }
    return val;
}
