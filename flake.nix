{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};

      hls = pkgs.haskell-language-server.override { supportedGhcVersions = [ "925" ]; };
    in
    {
      formatter.${system} = nixpkgs.legacyPackages.${system}.nixpkgs-fmt;

      devShells.${system}.default = pkgs.mkShell {
        packages = [
          pkgs.cabal-install
          pkgs.haskell.compiler.ghc925
          # hls
          pkgs.ormolu
        ];
      };
    };
}
