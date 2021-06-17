with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    stack
    haskell-language-server 
  ];
}
