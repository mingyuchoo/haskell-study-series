let
  pkgs = import <nixpkgs> {};
in
  pkgs.stdenv.mkDerivation {
    name = "haskell";
    buildInputs = [
      pkgs.haskell.compiler.ghc94
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.haskell-language-server
      pkgs.haskellPackages.stack
      pkgs.haskellPackages.stylish-haskell
      pkgs.haskellPackages.ghci
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.hindent
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.hoogle
    ];
    shellHook = ''
      echo "Welcome to nix-shell for Haskell!"
    '';
  }
