{ pkgs ? import <nixpkgs> {} }:

let
  emacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesFor emacs).emacsWithPackages;
  exwmOuterGaps =
    { trivialBuild
    , fetchFromGitHub
    , exwm
    , xelb
    } : trivialBuild rec {
      pname = "exwm-outer-gaps";
      version = "20240108";
      src = fetchFromGitHub {
        owner = "cherrypiejam";
        repo = "exwm-outer-gaps";
        rev = "06af6089aaed6acedc59e20c5810047eac613251";
        hash = "sha256-9kEBF2kIrUOPooeWryr9o5yUXDQCAjlnFmYba1CeNzQ=";
      };
      propagatedUserEnvPkgs = [ exwm xelb ];
      buildInputs = propagatedUserEnvPkgs;
    };
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    use-package
    doom-themes
    evil
    evil-collection
    magit
    ivy
    beacon
    which-key
    eldoc-box
    org-bullets
    pdf-tools
    nix-mode
    rust-mode
    markdown-mode
    scala-mode
    evil-org
  ])
  ++ (with epkgs.melpaPackages; [
    vterm
  ])
  ++ (with epkgs.elpaPackages; [
    exwm
    auctex
    org
    eglot
    company
    pinentry
  ])
  ++ (with epkgs; [
    tree-sitter
    (tree-sitter-langs.withPlugins (p: tree-sitter-langs.plugins ++ [
      p.tree-sitter-markdown
    ]))
    (callPackage exwmOuterGaps {
      inherit trivialBuild;
      inherit (elpaPackages) exwm xelb;
      inherit (pkgs) fetchFromGitHub;
    })
  ])
  ++ [
    # pkgs.notmuch
  ])
