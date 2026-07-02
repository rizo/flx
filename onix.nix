{ nixpkgs ? import <nixpkgs> {} }:
let
  onix = import (builtins.fetchGit {
    url = "https://github.com/rizo/onix.git";
    rev = "7fbdec45e3c6f8db2eb5fa0c41fcd94c67babf37";
  }) {
    verbosity = "info";
    ocamlPackages = nixpkgs.ocaml-ng.ocamlPackages_5_4;
  };

in onix.env {
  path = ./.;

  vars = {
    "with-test" = true;
    "with-doc" = true;
    "with-dev-setup" = true;
  };

  deps = {
    "ocaml-base-compiler" = "<5.5";
    "ocaml-lsp-server" = "*";
    "ocamlformat" = "*";
  };

  overlay = self: super: {
    dune = super.dune.overrideAttrs (oldAttrs: {
      buildInputs = [];
    });
  };
}

