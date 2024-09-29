{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs = [
    stack
    cabal-install
    ghc
    sqlite-interactive
    ffmpeg
    stylish-haskell
    hlint
    niv
    ghcid
    haskell-language-server
    haskellPackages.tasty-discover
    zlib
  ];

  shellHook = ''
    # ...
  '';
}
