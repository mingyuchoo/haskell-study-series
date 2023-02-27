# app-using-stack-on-nix

## Reference

- <https://docs.haskellstack.org/en/stable/nix_integration/>


## Prerequisit

### Install `nix`

```bash
$ curl -L https://nixos.org/nix/install | sh
```

### Install `stack` package to `nix`

```bash
$ nix-env -f "<nixpkgs>" -iA stack
```

## Create a new project

```bash
$ stack new <project-name> new-template
```

## Create a custom `shell.nix` file as specified bellow

```nix
{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [ glpk pcre ];
}
```
## Add a section to `stack.yaml` as follows

```yaml
nix:

  # false by default. Must be present and set to `true` to enable Nix, except on
  # NixOS where it is enabled by default (see #3938).  You can set set it in your
  # `$HOME/.stack/config.yaml` to enable Nix for all your projects without having
  # to repeat it
  enable: true

  # true by default. Tells Nix whether to run in a pure shell or not.
  pure: true

  # Empty by default. The list of packages you want to be
  # available in the nix-shell at build time (with `stack
  # build`) and run time (with `stack exec`).
  packages: []

  # Unset by default. You cannot set this option if `packages:`
  # is already present and not empty.
  shell-file: shell.nix

  # A list of strings, empty by default. Additional options that
  # will be passed verbatim to the `nix-shell` command.
  nix-shell-options: []

  # A list of strings, empty by default, such as
  # `[nixpkgs=/my/local/nixpkgs/clone]` that will be used to override
  # NIX_PATH.
  path: []

  # false by default. Whether to add your nix dependencies as nix garbage
  # collection roots. This way, calling nix-collect-garbage will not remove
  # those packages from the nix store, saving you some time when running
  # stack build again with nix support activated.
  # This creates a `nix-gc-symlinks` directory in the project `.stack-work`.
  # To revert that, just delete this `nix-gc-symlinks` directory.
  add-gc-roots: false
```

## Stack build and run

```bash
stack build --test --file-watch --watch--all
# or
# build more faster
$ stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS"

$ stack test --file-watch --watch--all
# or
# test automatically
$ ghcid -c="stack ghci test/Spec.hs"

$ stack run
```
