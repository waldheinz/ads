
#ifndef RIJNDAEL_H
#define RIJNDAEL_H

struct rijndael_sched_key;

int rijndael_sched_key_size();
void init_tables();

void rijndael_init_key(
   struct rijndael_sched_key*, const unsigned char*, const unsigned int, const unsigned int);

void encrypt_block(const struct rijndael_sched_key*,
   const unsigned char *, unsigned char *);

#endif

