
#ifndef LZMA_H
#define LZMA_H

#include "LzmaDec.h"

struct lzma_dec_state;

struct lzma_dec_state *lzma_dec_init(void* props);
void lzma_dec_free(struct lzma_dec_state *state);

#endif
