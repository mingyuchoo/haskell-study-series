let
  pkgs = import <nixpkgs> {};
in
  pkgs.stdenv.mkDerivation {
    name = "haskell";
    buildInputs = [
      pkgs.direnv
      pkgs.haskell.compiler.ghc925
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
      export EDITOR=emacs
      eval "$(direnv hook bash)"
      echo "Welcome to nix-shell for Haskell!"
    '';
  }
