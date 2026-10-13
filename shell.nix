{ pkgs ? import <nixpkgs> { } }:
with pkgs;
let
  hpkgs = haskellPackages;
  ghcWithDeps = hpkgs.ghcWithPackages (p: with p; [
    aeson
    base64-bytestring
    bytestring
    casing
    containers
    cryptonite
    exceptions
    finite-typelits
    generic-deriving
    http-client
    lens
    lens-aeson
    memory
    monad-logger
    mtl
    random
    retry
    tagsoup
    template-haskell
    text
    time
    unliftio-core
    vector
    wreq
    # test deps
    HUnit
    tasty
    tasty-hunit
    tasty-quickcheck
  ]);
in
mkShell {
  buildInputs = [
    ghcWithDeps
    stylish-haskell
    hlint
    ghcid
    hpkgs.haskell-language-server
    cabal-install
    hpack
    zlib
  ];

  shellHook = ''
    # ...
  '';
}
