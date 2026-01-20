let
  sources = {
    haskellNix = builtins.fetchTarball {
      name = "haskell-nix-snap";
      url = "https://github.com/input-output-hk/haskell.nix/archive/0db8595ce9ad5875bdfbada0ef0652bb1d4f5e86.tar.gz";
      sha256 = "07jdskdxm39n4kahb0bd1l09v84hgki23hn4b6kiqypynxjan2m6";
    };
  };

  haskellNix = import sources.haskellNix { };

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-unstable
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;

    ignoredPaths = ["docs" ".github"];
in pkgs.haskell-nix.stackProject {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.lib.cleanSourceWith {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "interval-int-map";
      src = ./.;
    };
    # ignore paths that change frequently, but do not contribute to the result
    filter = path: type: let baseName = baseNameOf (toString path); in !(pkgs.lib.elem baseName ignoredPaths);
  };

  # necessary to work around haskell.nix issue
  # see https://github.com/input-output-hk/haskell.nix/issues/2423
  modules = [
    {
      packages.directory.flags.os-string = true;
      packages.unix.flags.os-string = true;
      packages.process.flags.os-string = true;
    }
  ];
}

