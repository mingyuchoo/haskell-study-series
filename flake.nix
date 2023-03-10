{
  description = "A basic devShell using flake-utils for Haskell";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in
        rec {
          devShell = pkgs.mkShell {
            buildInputs = with pkgs; [
              direnv
              haskell.compiler.ghc925
              haskellPackages.cabal-install
              haskellPackages.haskell-language-server
              haskellPackages.stack
              haskellPackages.stylish-haskell
              haskellPackages.ghci
              haskellPackages.ghcid
              haskellPackages.hindent
              haskellPackages.hlint
              haskellPackages.hoogle
            ];
            shellHook = ''
              export LANG=C.UTF-8
              export EDITOR=emacs
              eval "$(direnv hook bash)"
              echo "Welcome to nix flake for Haskell"
            '';
          };
        }
    );
}
