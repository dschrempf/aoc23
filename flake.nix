{
  description = "Haskell development environment for the Advent of Code";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:
    let
      theseHpkgNames = [
        "aoc23"
      ];
      thisGhcVersion = "ghc94";
      hOverlay = selfn: supern: {
        haskell = supern.haskell // {
          packageOverrides = selfh: superh:
            supern.haskell.packageOverrides selfh superh //
              {
                aoc23 = selfh.callCabal2nix "aoc23" ./. { };
              };
        };
      };
      overlays = [ hOverlay ];
      perSystem = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            inherit overlays;
          };
          hpkgs = pkgs.haskell.packages.${thisGhcVersion};
          hlib = pkgs.haskell.lib;
          theseHpkgs = nixpkgs.lib.genAttrs theseHpkgNames (n: hpkgs.${n});
          theseHpkgsDev = builtins.mapAttrs (_: x: hlib.doBenchmark x) theseHpkgs;
        in
        {
          packages = theseHpkgs // { default = theseHpkgs.aoc23; };

          devShells.default = hpkgs.shellFor {
            packages = _: (builtins.attrValues theseHpkgsDev);
            nativeBuildInputs = with pkgs; [
              # Haskell toolchain.
              hpkgs.cabal-fmt
              hpkgs.cabal-install
              hpkgs.haskell-language-server
            ];
            buildInputs = with pkgs; [
            ];
            doBenchmark = true;
            # withHoogle = true;
          };
        };
    in
    { overlays.default = nixpkgs.lib.composeManyExtensions overlays; }
    // flake-utils.lib.eachDefaultSystem perSystem;
}
