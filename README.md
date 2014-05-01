# A Distributed Store

[![Build Status](https://travis-ci.org/waldheinz/ads.svg?branch=master)](https://travis-ci.org/waldheinz/ads)

This is a (partial) reimplementation of [Freenet][1], written from scratch in Haskell. It aims for compatibility in data formats, but *not* in the protocol.

## Features

  * TCP based protocol which allows data exchange between the nodes
  * can decode, encode and verify [CHK blocks][2]
  * can decode and verify [SSK blocks][3]
  * can decode most [split files][4] (except for very large ones, which use a metadata format which is not fully implemented)
  * can parse almost all of the weird archive & metadata formats which are mostly used to compose [Freesites][5]
  * there is a rudimentary Fproxy implementation which allows to view almost every Freesite in existence (well, at least the ones I dared to click on)
  * uses [Next Best Once][6] to make routing decisions (I hope I got it right)
  * decent performance

## Why you don't want to use it straight away

  * the node to node communication is neither encrypted nor secured against any form of impersonation or whatever evil you may think of
  * the FProxy implementation in no way filters the content it pulls from Freenet, and passes it straight to your browser. This means something as simple as an embedded image will reveal your identity to someone on the internet
  * I have only limited understanding of cryptography. Maybe this is not exactly the problem here, because I just reimplemented what Freenet does. I can't judge on the crypto expertise of Freenet developers / designers, so you're on you own with this.

[1]: https://freenetproject.org/
[2]: https://wiki.freenetproject.org/Content_Hash_Key
[3]: https://wiki.freenetproject.org/SSK
[4]: https://wiki.freenetproject.org/Splitfile
[5]: https://wiki.freenetproject.org/Freesite
[6]: http://arxiv.org/abs/1401.2165
