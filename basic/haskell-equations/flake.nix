{
  description = "A Hello World in Haskell with a dependency and a devShell";

  inputs.nixpkgs.url = "nixpkgs";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });

      packageName = "haskell-equations";
    in
    {
      overlay = (final: prev: {
        ${packageName} = final.haskellPackages.callCabal2nix "${packageName}" ./. {};
      });

      packages = forAllSystems (system: {
         ${packageName} = nixpkgsFor.${system}.${packageName};
      });

      defaultPackage = forAllSystems (system: self.packages.${system}.${packageName});

      checks = self.packages;

      devShell = forAllSystems (system:
        let
          haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in
          haskellPackages.shellFor {
            packages = p: [self.packages.${system}.${packageName}];

            withHoogle = true;

            buildInputs = with haskellPackages; [
              direnv
              cabal-install
              haskell-language-server
              ghcid
              lsp-hasekll
            ];

            shellHook = ''
              export LANG=C.UTF-8
              export EDITOR=emacs
              export PS1="\\e[1;34m(develop) \u@\h:\W > \\e[0m"
              eval "$(direnv hook bash)"
              echo "Welcome to nix flake for Python3"
            '';
        });
  };
}
