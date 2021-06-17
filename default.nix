with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "c4c";
  src = ./.;
  buildInputs = [];
}
