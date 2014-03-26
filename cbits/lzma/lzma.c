
#include <stdlib.h>
#include <string.h>
#include "lzma.h"

struct lzma_dec_state {
  CLzmaDec decoder;
  ISzAlloc alloc;
};

void *alloc_impl(void *p, size_t size) {
  p = p;
  return malloc(size);
}

void free_impl(void *p, void* addr) {
  p = p;
  free(addr);
}

struct lzma_dec_state *lzma_dec_init(void* props) {
  struct lzma_dec_state *state = malloc(sizeof(struct lzma_dec_state));
  memset(state, 0, sizeof(*state));

  if (!state) {
    return NULL;
  }
  
  state->alloc.Alloc = &alloc_impl;
  state->alloc.Free = &free_impl;

  if (LzmaDec_Allocate(&state->decoder, props, 5, &state->alloc)) {
    free (state);
    return NULL;
  }

  LzmaDec_Init(&state->decoder);

  return state;
}

void lzma_dec_free(struct lzma_dec_state *state) {
  LzmaDec_Free(&state->decoder, &state->alloc);
  free(state);
}

void* lzma_decode(struct lzma_dec_state *state, void *src, size_t src_len, size_t *dest_len) {
  ELzmaStatus status;
  size_t current_size = 4096;
  void *target = malloc(current_size);
  *dest_len = 0;
  int done = 0;
  size_t pos_in = 0;

  while (!done) {
    size_t remain_out = current_size - *dest_len;
    size_t remain_in  = src_len - pos_in;
    
    LzmaDec_DecodeToBuf(
			&state->decoder,
			target + *dest_len, &remain_out,
			src    + pos_in , &remain_in,
			LZMA_FINISH_END,
			&status);
    
    pos_in  += remain_in;
    *dest_len += remain_out;

    switch (status) {
    case LZMA_STATUS_NOT_FINISHED:
      current_size *= 2;
      target = realloc(target, current_size);
      break;
    default:
      done = 1;
    }
  }
  
  return target;
}
