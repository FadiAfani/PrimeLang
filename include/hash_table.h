#ifndef _HASH_TABLE_H_
#define _HASH_TABLE_H_

#include <stdint.h>
#include "vh_common.h"

uint16_t hash(uint16_t n, uint16_t m);
void insert(HashTable* hash_table, uint16_t key, Value val);
Value lookup(HashTable* hash_table, uint16_t key);
/* hash table of known size 
 * use a larger capacity to avoid frequent collisions
 * */
HashTable* init_hash_table(uint16_t num_keys);

#endif
