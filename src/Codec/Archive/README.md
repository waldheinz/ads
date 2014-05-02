
# So I made a copy of the tar package

This is a straight copy of the [tar package][1], with only one
minor addition: The magic ["ustar\0\0\0"][2] is recognized as
`GnuFormat`. I don't know if Freenet creates bogus tar files or
this is missing in the tar package. Yet, this way it works.

[1]: http://hackage.haskell.org/package/tar
[2]: https://github.com/waldheinz/ads/blob/master/src/Codec/Archive/Tar/Read.hs#L141
