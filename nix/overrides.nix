{ haskell }:
self: _:
{
  path = haskell.lib.buildStrictly (self.callPackage ../path { });
}
