{
  description = "Self-hosting parser generators in Forth and C.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with import nixpkgs { inherit system; }; {
        defaultPackage = stdenv.mkDerivation {
          name = "meta-yacc";
          nativeBuildInputs = [ gforth ];
          src = ./.;
        };
      }
    );
}
