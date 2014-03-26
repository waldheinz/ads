
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lzma.h"

struct lzma_dec_state {
  CLzmaDec decoder;
  ISzAlloc alloc;
};

void *alloc_impl(void *p, size_t size) {
  p = p;
  printf("size: %i\n", size);
  return malloc(size);
}

void free_impl(void *p, void* addr) {
  p = p;
  free(addr);
}

struct lzma_dec_state *lzma_dec_init(void* props) {
  printf("nei\n");
  struct lzma_dec_state *state = malloc(sizeof(struct lzma_dec_state));
  memset(state, 0, sizeof(*state));
  printf("malloc\n");

  if (!state) {
    return NULL;
  }
  
  state->alloc.Alloc = &alloc_impl;
  state->alloc.Free = &free_impl;

  printf("lz_alloc\n");
  printf("props: %x\n", props);
  if (LzmaDec_Allocate(&state->decoder, props, 5, &state->alloc)) {
    printf("alloc failed\n");
    free (state);
    return NULL;
  }
  
  printf("lz_alloc ok\n");

  LzmaDec_Init(&state->decoder);

  printf("created!\n");

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
    
    int ret = LzmaDec_DecodeToBuf(
			&state->decoder,
			target + *dest_len, &remain_out,
			src    + pos_in , &remain_in,
			LZMA_FINISH_END,
			&status);
    
    pos_in  += remain_in;
    *dest_len += remain_out;
    
    printf ("%d %d %d\n", status, remain_in, remain_out);

    switch (status) {
    case LZMA_STATUS_NOT_FINISHED:
      current_size *= 2;
      target = realloc(target, current_size);
      break;
    case LZMA_STATUS_NEEDS_MORE_INPUT:
      printf("more input\n");
      break;
    default:
      done = 1;
    }
  }

  printf("decoded %d (size = %d)\n", *dest_len, current_size);
  return target;
}
