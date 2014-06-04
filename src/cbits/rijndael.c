
/*
 * shameless plug from here: http://www.lomont.org/Software/Misc/AES/AES.php
 */

#include "rijndael.h"

#include <stdint.h>

// tables for inverses, byte sub
unsigned char gf2_8_inv[256];
unsigned char byte_sub[256];
unsigned char inv_byte_sub[256];

// this table needs Nb*(Nr+1)/Nk entries - up to 8*(15)/4 = 60
// todo - remove table, note cycles every 17(?) elements
uint32_t Rcon[60];

// long tables for encryption stuff
uint32_t T0[256];
uint32_t T1[256];
uint32_t T2[256];
uint32_t T3[256];

// long tables for decryption stuff
uint32_t I0[256];
uint32_t I1[256];
uint32_t I2[256];
uint32_t I3[256];

// huge tables - todo - ifdef out
uint32_t T4[256];
uint32_t T5[256];
uint32_t T6[256];
uint32_t T7[256];
uint32_t I4[256];
uint32_t I5[256];
uint32_t I6[256];
uint32_t I7[256];

inline unsigned char GF2_8_mult(unsigned char a, unsigned char b) {
	unsigned char result = 0;
   
	int count = 8;
	
	while (count--) {
		if (b&1)
			result ^= a;
		if (a&128)
			{
			a <<= 1;
			a ^= (0x1B);
			}
		else
			a <<= 1;
		b >>= 1;
		}
	return result;
}

void create_inverses() {   
	gf2_8_inv[0] = 0;
	
	for (int a = 1; a <= 255; a++) {
      unsigned int b = 1;
      
      while (GF2_8_mult(a,b) != 1) {
         b++;
      }
      
      gf2_8_inv[a] = b;
   }
}

/* the sum of bits mod 2 */
unsigned char bit_sum(unsigned char byte) {
   byte = (byte >> 4) ^ (byte & 15);
   byte = (byte >> 2) ^ (byte & 3);
   return (byte >> 1) ^ (byte & 1);
}

void create_byte_sub() {
	for (int x = 0; x <= 255; x++) {
		unsigned int y = gf2_8_inv[x]; // inverse to start with
		
		// affine transform
		y = bit_sum(y&0xF1) | (bit_sum(y&0xE3)<<1) | (bit_sum(y&0xC7)<<2) | (bit_sum(y&0x8F)<<3) |
			(bit_sum(y&0x1F)<<4) | (bit_sum(y&0x3E)<<5) | (bit_sum(y&0x7C)<<6) | (bit_sum(y&0xF8)<<7);
		y = y ^ 0x63;
		byte_sub[x] = y;
   }
}

void create_inv_byte_sub()	{
   for (int x = 0; x <= 255; x++) {
      unsigned int y = 0;
      
      while (byte_sub[y] != x) {
         y++;
      }
      
      inv_byte_sub[x] = y;
   }
}

void create_rcon() {
   unsigned char Ri = 1; // start here

   Rcon[0] = 0;
		
	for (int i = 1; i < sizeof(Rcon)/sizeof(Rcon[0])-1; i++) {
		Rcon[i] = Ri;
      Ri = GF2_8_mult(Ri,0x02); // multiply by x - todo replace with xmult
   }
}

// define to mult a byte by x mod the proper poly
// todo - move magic numbers out?
#define xmult(a) ((a)<<1) ^ (((a)&128) ? 0x01B : 0)

// make 4 bytes (LSB first) into a 4 byte vector
#define VEC4(a,b,c,d) (((uint32_t)(a)) | (((uint32_t)(b))<<8) | (((uint32_t)(c))<<16) | (((uint32_t)(d))<<24))
#define ROTATE_LEFT(x, n) (((x) << (n)) | ((x) >> (32-(n))))
#define ROTATE_BYTE_LEFT(a) ROTATE_LEFT(a,8)
#define ROTATE_RIGHT(x, n) (((x) >> (n)) | ((x) << (32-(n))))
#define ROTATE_BYTE_RIGHT(a) ROTATE_RIGHT(a,8)

void create_large_tables() {
	unsigned int i;
	unsigned char a1,a2,a3,b1,b2,b3,b4,b5;
   
	for (i = 0; i < 256; i++) {
		a1 = byte_sub[i];
		a2 = xmult(a1);
		a3 = a2^a1;

		b5 = inv_byte_sub[i];
		b1 = GF2_8_mult(0x0E,b5);
		b2 = GF2_8_mult(0x09,b5);
		b3 = GF2_8_mult(0x0D,b5);
		b4 = GF2_8_mult(0x0B,b5);

      T0[i] = VEC4(a2,a1,a1,a3);
      T1[i] = ROTATE_BYTE_LEFT(T0[i]);
      T2[i] = ROTATE_BYTE_LEFT(T1[i]);
      T3[i] = ROTATE_BYTE_LEFT(T2[i]);

      T4[i] = VEC4(a1,0,0,0); // identity
      T5[i] = ROTATE_BYTE_LEFT(T4[i]);
      T6[i] = ROTATE_BYTE_LEFT(T5[i]);
      T7[i] = ROTATE_BYTE_LEFT(T6[i]);
      
      I0[i] = VEC4(b1,b2,b3,b4);
      I1[i] = ROTATE_BYTE_LEFT(I0[i]);
      I2[i] = ROTATE_BYTE_LEFT(I1[i]);
      I3[i] = ROTATE_BYTE_LEFT(I2[i]);

      I4[i] = VEC4(b5,0,0,0); // identity
      I5[i] = ROTATE_BYTE_LEFT(I4[i]);
      I6[i] = ROTATE_BYTE_LEFT(I5[i]);
      I7[i] = ROTATE_BYTE_LEFT(I6[i]);
   }
}

void init_tables() {
   create_inverses();
   create_byte_sub();
   create_inv_byte_sub();
   create_rcon();
   create_large_tables();
}

struct rijndael_sched_key {
   int Nb,Nk;    // block and key length / 32, should be 4,6,or 8
	int Nr;       // number of rounds
	unsigned char W[4*8*15];   // the expanded key
};

int rijndael_sched_key_size() {
   return sizeof(struct rijndael_sched_key);
}

/* does the SBox on this 4 byte data */
uint32_t sub_byte(uint32_t data)	{ 
   uint32_t result = 0;
   
   result = byte_sub[data>>24];
   result <<= 8;
   result |= byte_sub[(data>>16)&255];
   result <<= 8;
   result |= byte_sub[(data>>8)&255];
   result <<= 8;
   result |= byte_sub[data&255];
   
   return result;
}

void rijndael_init_key(
   struct rijndael_sched_key* sched,
   const unsigned char* key,
   const unsigned int nb, const unsigned int nk) {
   int i;
   
	static int const parameters[] = {
		10, 	12,  	14,  // Nb*32 = 128
		12, 	12,  	14,  // Nb*32 = 192
		14, 	14,  	14,  // Nb*32 = 256
		};
	
   sched->Nk = nk;
   sched->Nb = nb;
   sched->Nr = parameters[((nk-4)/2 + 3*(nb-4)/2)];
   const int nr = sched->Nr;
   
   /* key expansion */
   
	uint32_t temp, *Wb = (uint32_t*)&(sched->W);
	if (nk <= 6)
		{
		// todo - memcpy
		for (i = 0; i < 4*nk; i++)
			sched->W[i] = key[i];
		for (i = nk; i < nb*(nr+1); i++)
			{
			temp = Wb[i-1];
			if ((i%nk) == 0)
				temp = sub_byte(ROTATE_BYTE_RIGHT(temp)) ^ Rcon[i / nk];
			Wb[i] = Wb[i - nk]^temp;
			}
		}
	else
		{
		// todo - memcpy
		for (i = 0; i < 4*nk; i++)
			sched->W[i] = key[i]; 
		for (i = nk; i < nb*(nr+1); i++)
			{
			temp = Wb[i-1];
			if ((i % nk) == 0)
				temp = sub_byte(ROTATE_BYTE_RIGHT(temp)) ^ Rcon[i / nk];
			else if ((i % nk) == 4)
				temp = sub_byte(temp);
			Wb[i] = Wb[i - nk] ^ temp;
			}
		}
}


// key adding for 4,6,8 column cases
#define AddRoundKey4(dest,src)	\
	*dest++ = *r_ptr++ ^ *src++;\
	*dest++ = *r_ptr++ ^ *src++;\
	*dest++ = *r_ptr++ ^ *src++;\
	*dest++ = *r_ptr++ ^ *src++;

#define AddRoundKey6(dest,src)	\
	*dest++ = *r_ptr++ ^ *src++;\
	*dest++ = *r_ptr++ ^ *src++;\
	*dest++ = *r_ptr++ ^ *src++;\
	*dest++ = *r_ptr++ ^ *src++;\
	*dest++ = *r_ptr++ ^ *src++;\
	*dest++ = *r_ptr++ ^ *src++;

#define AddRoundKey8(dest,src)	\
	*dest++ = *r_ptr++ ^ *src++;\
	*dest++ = *r_ptr++ ^ *src++;\
	*dest++ = *r_ptr++ ^ *src++;\
	*dest++ = *r_ptr++ ^ *src++;\
	*dest++ = *r_ptr++ ^ *src++;\
	*dest++ = *r_ptr++ ^ *src++;\
	*dest++ = *r_ptr++ ^ *src++;\
	*dest++ = *r_ptr++ ^ *src++;

// this define computes one of the round vectors 
#define compute_one(dest,src2,j,C1,C2,C3,Nb)	*(dest+j) = \
	T0[GetByte(src2[j],0)]^T1[GetByte(src2[((j+C1+Nb)%Nb)],1)]^ \
	T2[GetByte(src2[((j+C2+Nb)%Nb)],2)]^T3[GetByte(src2[((j+C3+Nb)%Nb)],3)] \
	^*r_ptr++

// single table version, slower
#define compute_one_small(dest,src2,j,C1,C2,C3,Nb)	*(dest+j) = *r_ptr++^\
	T0[GetByte(src2[j],0)]^\
	RotByteL(T0[GetByte(src2[((j+C1+Nb)%Nb)],1)]^\
	RotByteL(T0[GetByte(src2[((j+C2+Nb)%Nb)],2)]^\
	RotByteL(T0[GetByte(src2[((j+C3+Nb)%Nb)],3)])))

#define Round4(d,s)	\
		compute_one(d,s,0,1,2,3,4); \
		compute_one(d,s,1,1,2,3,4);	\
		compute_one(d,s,2,1,2,3,4);	\
		compute_one(d,s,3,1,2,3,4); 

#define Round6(d,s) \
		compute_one(d,s,0,1,2,3,6); \
		compute_one(d,s,1,1,2,3,6); \
		compute_one(d,s,2,1,2,3,6); \
		compute_one(d,s,3,1,2,3,6); \
		compute_one(d,s,4,1,2,3,6); \
		compute_one(d,s,5,1,2,3,6);

#define Round8(d,s) \
		compute_one(d,s,0,1,3,4,8); \
		compute_one(d,s,1,1,3,4,8); \
		compute_one(d,s,2,1,3,4,8); \
		compute_one(d,s,3,1,3,4,8); \
		compute_one(d,s,4,1,3,4,8); \
		compute_one(d,s,5,1,3,4,8); \
		compute_one(d,s,6,1,3,4,8); \
		compute_one(d,s,7,1,3,4,8); 

#define compute_one_inv(dest,src2,j,C1,C2,C3,Nb)	*(dest+j) = \
	I0[GetByte(src2[j],0)]^I1[GetByte(src2[((j-C1+Nb)%Nb)],1)]^ \
	I2[GetByte(src2[((j-C2+Nb)%Nb)],2)]^I3[GetByte(src2[((j-C3+Nb)%Nb)],3)] \
	^*r_ptr++

#define InvRound4(d,s)	\
		compute_one_inv(d,s,0,1,2,3,4); \
		compute_one_inv(d,s,1,1,2,3,4);	\
		compute_one_inv(d,s,2,1,2,3,4);	\
		compute_one_inv(d,s,3,1,2,3,4); 

#define InvRound6(d,s)	\
		compute_one_inv(d,s,0,1,2,3,6); \
		compute_one_inv(d,s,1,1,2,3,6);	\
		compute_one_inv(d,s,2,1,2,3,6);	\
		compute_one_inv(d,s,3,1,2,3,6);	\
		compute_one_inv(d,s,4,1,2,3,6);	\
		compute_one_inv(d,s,5,1,2,3,6); 

#define InvRound8(d,s)	\
		compute_one_inv(d,s,0,1,3,4,8); \
		compute_one_inv(d,s,1,1,3,4,8);	\
		compute_one_inv(d,s,2,1,3,4,8);	\
		compute_one_inv(d,s,3,1,3,4,8);	\
		compute_one_inv(d,s,4,1,3,4,8);	\
		compute_one_inv(d,s,5,1,3,4,8);	\
		compute_one_inv(d,s,6,1,3,4,8);	\
		compute_one_inv(d,s,7,1,3,4,8); 

// this define computes one of the final round vectors
#define compute_one_final1(dest,src,j,C1,C2,C3,Nb)  *dest++ = \
	(T3[GetByte(src[j],0)]&0xFF)^\
	(T3[GetByte(src[((j+C1+Nb)%Nb)],1)]&0xFF00)^ \
	(T1[GetByte(src[((j+C2+Nb)%Nb)],2)]&0xFF0000)^ \
	(T1[GetByte(src[((j+C3+Nb)%Nb)],3)]&0xFF000000)^*r_ptr++

// for another 4K tables, we save 3 clock cycles - sick
#define compute_one_final(dest,src,j,C1,C2,C3,Nb)  *dest++ = \
	(T4[GetByte(src[j],0)])^\
	(T5[GetByte(src[((j+C1+Nb)%Nb)],1)])^ \
	(T6[GetByte(src[((j+C2+Nb)%Nb)],2)])^ \
	(T7[GetByte(src[((j+C3+Nb)%Nb)],3)])^*r_ptr++

// final round defines - this one is for case for 4 columns
#define FinalRound4(d,s) compute_one_final(d,s,0,1,2,3,4); \
						compute_one_final(d,s,1,1,2,3,4); \
						compute_one_final(d,s,2,1,2,3,4); \
						compute_one_final(d,s,3,1,2,3,4);

#define FinalRound6(d,s) compute_one_final(d,s,0,1,2,3,6); \
						compute_one_final(d,s,1,1,2,3,6); \
						compute_one_final(d,s,2,1,2,3,6); \
						compute_one_final(d,s,3,1,2,3,6); \
						compute_one_final(d,s,4,1,2,3,6); \
						compute_one_final(d,s,5,1,2,3,6);

#define FinalRound8(d,s) compute_one_final(d,s,0,1,3,4,8); \
						compute_one_final(d,s,1,1,3,4,8); \
						compute_one_final(d,s,2,1,3,4,8); \
						compute_one_final(d,s,3,1,3,4,8); \
						compute_one_final(d,s,4,1,3,4,8); \
						compute_one_final(d,s,5,1,3,4,8); \
						compute_one_final(d,s,6,1,3,4,8); \
						compute_one_final(d,s,7,1,3,4,8);

// inverse cipher stuff
#define compute_one_final_inv(dest,src,j,C1,C2,C3,Nb)  *dest++ = \
	(I4[GetByte(src[j],0)])^\
	(I5[GetByte(src[((j-C1+Nb)%Nb)],1)])^ \
	(I6[GetByte(src[((j-C2+Nb)%Nb)],2)])^ \
	(I7[GetByte(src[((j-C3+Nb)%Nb)],3)])^*r_ptr++

// final round defines - this one is for case for 4 columns
#define InvFinalRound4(d,s) compute_one_final_inv(d,s,0,1,2,3,4); \
						compute_one_final_inv(d,s,1,1,2,3,4); \
						compute_one_final_inv(d,s,2,1,2,3,4); \
						compute_one_final_inv(d,s,3,1,2,3,4);

#define InvFinalRound6(d,s) compute_one_final_inv(d,s,0,1,2,3,6); \
						compute_one_final_inv(d,s,1,1,2,3,6); \
						compute_one_final_inv(d,s,2,1,2,3,6); \
						compute_one_final_inv(d,s,3,1,2,3,6); \
						compute_one_final_inv(d,s,4,1,2,3,6); \
						compute_one_final_inv(d,s,5,1,2,3,6);

#define InvFinalRound8(d,s) compute_one_final_inv(d,s,0,1,3,4,8); \
						compute_one_final_inv(d,s,1,1,3,4,8); \
						compute_one_final_inv(d,s,2,1,3,4,8); \
						compute_one_final_inv(d,s,3,1,3,4,8); \
						compute_one_final_inv(d,s,4,1,3,4,8); \
						compute_one_final_inv(d,s,5,1,3,4,8); \
						compute_one_final_inv(d,s,6,1,3,4,8); \
						compute_one_final_inv(d,s,7,1,3,4,8); 

#define GetByte(a,n) ((unsigned char)((a) >> (n<<3)))

void encrypt_block(const struct rijndael_sched_key* sched,
   const unsigned char * datain1,
   unsigned char * dataout1) {
	  // todo - clean up - lots of repeated macros
	  // we only encrypt one block from now on
   
	uint32_t state[8*2]; // 2 buffers
	uint32_t * r_ptr = (uint32_t*)(sched->W);
	uint32_t * dest  = state;
	uint32_t * src   = state; 
	const uint32_t * datain = (const uint32_t*)(datain1);
	uint32_t * dataout = (uint32_t*)(dataout1);
   const int Nb = sched->Nb;
   const int Nr = sched->Nr;
   
	if (Nb == 4)
		{
		AddRoundKey4(dest,datain);

		if (Nr == 14)
			{
			Round4(dest,src);
			Round4(src,dest);
			Round4(dest,src);
			Round4(src,dest);
			}
		else if (Nr == 12)
			{
			Round4(dest,src);
			Round4(src,dest);
			}

		Round4(dest,src);
		Round4(src,dest);
		Round4(dest,src);
		Round4(src,dest);
		Round4(dest,src);
		Round4(src,dest);
		Round4(dest,src);
		Round4(src,dest);
		Round4(dest,src);

		FinalRound4(dataout,dest);
		}
	else if (Nb == 6)
		{
		AddRoundKey6(dest,datain);

		if (Nr == 14)
			{
			Round6(dest,src);
			Round6(src,dest);
			}

		Round6(dest,src);
		Round6(src,dest);
		Round6(dest,src);
		Round6(src,dest);
		Round6(dest,src);
		Round6(src,dest);
		Round6(dest,src);
		Round6(src,dest);
		Round6(dest,src);
		Round6(src,dest);
		Round6(dest,src);

		FinalRound6(dataout,dest);
		}
	else // Nb == 8
		{
		AddRoundKey8(dest,datain);

		Round8(dest,src);
		Round8(src,dest);
		Round8(dest,src);
		Round8(src,dest);
		Round8(dest,src);
		Round8(src,dest);
		Round8(dest,src);
		Round8(src,dest);
		Round8(dest,src);
		Round8(src,dest);
		Round8(dest,src);
		Round8(src,dest);
		Round8(dest,src);

		FinalRound8(dataout,dest);
		} // end switch on Nb

	} // Encrypt

