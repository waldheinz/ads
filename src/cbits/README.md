# Why this is needed

## Rijndael

Freenet makes use of the Rijndael block ciphre, which is *not* the same as
AES. AES is a subset of Rijndael, not supporting the 256 bit block size
used by Freenet in some places:

  * There are two variants of CHK blocks, and the older version makes use of
  Rijndael. Those blocks are still floating around and can be decrypted using
  this code. The newer variant does use plain AES, and only such blocks are
  ever created when inserting new data. So this code is used only for decoding,
  and will eventually be obsoleted when the last old CHK block has fallen out
  of the stores.

  * The original UDP - based Freenet protocol makes extensive use of this,
  which is not really a concern for ADS as it does not support this protocol
  currently.

## LZMA

LZMA is an older variant of what is now known as XZ, also used by Freenet as one
possible compression format. The [lzma-conduit][1] package only supports the newer
format, so I pulled this in. Having a proper conduit wrapper for this would be
key to allowing download of larger files from Freenet, as the current code does
not support streaming decompression.

[1]: http://hackage.haskell.org/package/lzma-conduit
