# A Distributed Store

[![Build Status](https://travis-ci.org/waldheinz/ads.svg?branch=master)](https://travis-ci.org/waldheinz/ads)

This is a (partial) reimplementation of [Freenet][1], written from scratch in Haskell. It aims for compatibility in data formats, but *not* in the protocol.

## Why you don't want to use it straight away

  * The node to node communication is neither encrypted nor secured against any form of impersonation or whatever evil you may think of.
  * The FProxy implementation in no way filters the content it pulls from Freenet, and passes it straight to your browser. This means something as simple as an embedded image will reveal your identity to someone on the internet.
  * I have only limited understanding of cryptography. Maybe this is not exactly the problem here, because I just reimplemented what Freenet does. I can't judge on the crypto expertise of Freenet developers / designers, so you're on you own with this.
  * This is very young code, and many things are still in flux and rough around the edges. If you're not a "developer" chances are you won't get it to run (or shoot yourself in the foot while trying).


## Give it a try anyway

Here's what needs to be done:

  * Check out and compile the code, preferably in a cabal sandbox.
  * Start it up for the first time with `cabal run`, and shut it down again when you see the `node listening on ...` message. This will initialize a `~/.ads` directory containing:
    * A `config` file with some basic options in [configurator][configurator] format. You may want to adjust
      * `node.listen`: where your node listens for node-to-node connections
      * `node.http` : there is some rudimentary web interface / REST API, this allows you to tune where this listens
      * `fproxy`: this is where the FProxy allows to browse Freesites. This is currently separate from `node.http` for ... reasons. It's likely to be merged with the former soon.
      * `freenet.datastore`: the directory where the datastore lives, and how big you want it to be. Probably you want to change the defaults, here are some things to keep in mind:
        * Currently it is not possible to grow/shrink the data store. When you decide you want to change it, all data will be lost. Don't be that guy.
        * These files are created as sparse files by default. This means they won't take up space until data is actually written there, which is good. It also means that they'll likely be heavily fragmented when they have grown to a rasonable size, which is bad. If you can afford the space, just delete the freshly created files and recreate them with the `fallocate` command. This is quick and really makes a difference performance-wise. Also, COW file systems like ZFS and probably BTRFS tend to fragment these files as well. Good results can be obtained with the `ext4` + `fallocate` combo. If your store lives on an SSD you don't have to worry about fragmentation, only about the size.
        * The `*histogram` files are re-created when lost, but this can take some time when the store is large.
    * A `identity` file in JSON format, containing
      * the `id` of your node, which is just 32 random bytes. It's not particulary important what that id is, but changing it after the first connection to the network might cause [severe problems][id-mismatch] finding nodes willing to talk to you. Just let it alone.
      * an `addresses` list, defining where your node can be reached by other nodes. This list can be empty, which means your node will only ever make outgoing connections. If at all possible, you should put some adresses there, each in the following format: `{ "host" : "<IP address or hostname>", "port" : <port number> }`. The port number will likely be the same you used in `node.listen` in the `config` file, but may be different when using port forwardings. For the hostname, anything goes: plain IPv4 and IPv6 adresses, as well as DNS names. When other nodes try to connect to your node, they will try the adresses in the order you put them in this list. This is useful when your node can be reached over the Internet and over some LAN: Just put the local address before the Internet address, and other nodes on the same network will prefer the faster link; maybe a laptop talking to the desktop PC.
    * You'll also need a `peers` file to make some connections: These are the peers your node will initally connect to, and then it will learn about other nodes from the nodes it has connected to, and so on. [Here][seed-nodes] is one to get you started.
  * When all this works, you may point your FProxy to Enzo's index at `USK@XJZAi25dd5y7lrxE3cHMmM-xZ-c-hlPpKLYeLC0YG5I,8XTbR1bd9RBXlX6j-OZNednsJ8Cl6EAeBBebC3jtMFU,AQACAAE/index/336/`. (I do not know who Enzo is, and I don't endorse any of the content on his site. It's just an up-to-date index containing links to other Freesites you may or may not like. Which I may or may not like. Which may or may not be legal in your country.)
  
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
[configurator]: http://hackage.haskell.org/package/configurator
[id-mismatch]: https://github.com/waldheinz/ads/blob/master/src/Node.hs#L417
[seed-nodes]: https://gist.github.com/waldheinz/317bfacd16eab84099f1
