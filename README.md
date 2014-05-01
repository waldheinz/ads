# A Distributed Store

[![Build Status](https://travis-ci.org/waldheinz/ads.svg?branch=master)](https://travis-ci.org/waldheinz/ads)

This is a (partial) reimplementation of [Freenet][1], written from scratch in Haskell. It aims for compatibility in data formats, but *not* in the protocol.

## Why you don't want to use it straight away

  * The node to node communication is neither encrypted nor secured against any form of impersonation or whatever evil you may think of.
  * The FProxy implementation in no way filters the content it pulls from Freenet, and passes it straight to your browser. This means something as simple as an embedded image will reveal your identity to someone on the internet.
  * I have only limited understanding of cryptography. Maybe this is not exactly the problem here, because I just reimplemented what Freenet does. I can't judge on the crypto expertise of Freenet developers / designers, so you're on you own with this.

## Features

  * TCP based protocol which allows data exchange between the nodes
  * can decode, encode and verify [CHK blocks][2]
  * can decode and verify [SSK blocks][3]
  * can decode most [split files][4] (except for very large ones, which use a metadata format which is not fully implemented)
  * can parse almost all of the weird archive & metadata formats which are mostly used to compose [Freesites][5]
  * there is a rudimentary Fproxy implementation which allows to view almost every Freesite in existence (well, at least the ones I dared to click on)
  * uses [Next Best Once][6] to make routing decisions (I hope I got it right)
  * decent performance and low resource usage. The latter is mainly true for nodes where the FProxy is not used (which are only forwarding and storing data). The FProxy could use some more love, though.

## Interop with Freenet

Interop with freenet mainly means we need a way to pull data blocks from Freenet into the ADS network and vice versa.

### From Freenet to ADS

This works quite well, and is implemented by the means of a [Freenet plugin][7]. The plugin listens on a TCP socket, and an ADS node can be configured to connect there. ADS nodes which have such an companion configured will ask their Freenet counterpart if they can't find some data locally.

Setting this up is a bit tendious at first, but at least it runs quite stable unattended and gives decent performance. I have a battery of 16 Freenet companion nodes evenly distibuted across the keyspace, which allow data to migrate from Freenet to ADS. This gives impressive performance compared to what a single Freenet node can provide, no matter how many resources you throw at it.

### From ADS to Freenet

I have a partial implementation of this, in the form of a patched Freenet node. This node will connect to other Freenet nodes just as usual. But all incoming requests are not forwarded to other Freenet nodes, but are translated to HTTP requests against an ADS node's REST API. This is working well so far, but the problem is that the Freenet node is not sending the data it gets from ADS onward to whichever Freenet node requested it. This means the data has to be requested twice from the Freenet side to be actually routed:

  * the first time it finds it's way from ADS to the pached Freenet node's data store and
  * the second time this node finds it in it's store and it's actually routed within Freenet

This is far from optimal, and probably someone more familiar with the Freenet codebase can easily fix this. Maybe.

[1]: https://freenetproject.org/
[2]: https://wiki.freenetproject.org/Content_Hash_Key
[3]: https://wiki.freenetproject.org/SSK
[4]: https://wiki.freenetproject.org/Splitfile
[5]: https://wiki.freenetproject.org/Freesite
[6]: http://arxiv.org/abs/1401.2165
[7]: https://github.com/waldheinz/ads-companion
