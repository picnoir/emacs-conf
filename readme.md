# Personal Emacs Configuration

## OS Glue

Feeling adventurous? Feeling like using your host OS to keep your Emacs packages at the cost of a terribly slow feedback loop?

Use this.

### Nix

```nix
{ emacsWithPackagesFromUsePackage, pkgs, ispell, stdenvNoCC, sources }:
let
  source = import ../nix/sources.nix;
in emacsWithPackagesFromUsePackage {
  config = "${sources.emacs-conf}/default.el";
  defaultInitFile = true;
  package = pkgs.emacsNativeComp.override {
    withGTK3 = true;
  };
  alwaysEnsure = true;
  extraEmacsPackages = epkgs: [
    epkgs.monokai-theme
    epkgs.protobuf-mode
  ];
}
```

### Guix

TODO: I temporarily gave up on the Guix emacs build system and imperatively use this conf from there.
