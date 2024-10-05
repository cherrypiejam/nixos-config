{ emacs, emacsPackagesFor, callPackage, fetchFromGitHub }:

let
  # Using the latest exwm
  emacsPackages = (emacsPackagesFor emacs).overrideScope (eself: esuper: {
    xelb = callPackage ({ trivialBuild, compat, emacs, fetchFromGitHub }:
      trivialBuild rec {
        pname = "xelb";
        version = "0.20.1";
        src = fetchFromGitHub {
          owner = "emacs-exwm";
          repo = "xelb";
          rev = "25589d15117c9e13ce804b8b446bdd5ba25d52e3";
          hash = "sha256-qYogTR//di+qOFwTNbCNJG9vfx/MusljbTd4ojPyFgg=";
        };
        propagatedUserEnvPkgs = [ compat emacs ];
        buildInputs = propagatedUserEnvPkgs;
      }
    ) { inherit (esuper) trivialBuild compat; };
    exwm = callPackage ({ trivialBuild, compat, emacs, xelb, fetchFromGitHub }:
      trivialBuild rec {
        pname = "exwm";
        version = "0.31.1";
        src = fetchFromGitHub {
          owner = "emacs-exwm";
          repo = "exwm";
          rev = "f00b5ca655a0471a10d21a3e75b1a442a8d28941";
          hash = "sha256-OfrY76s9/oIVk7PlWM2/9bYuTSH8evRXoZh4L8RF7CY=";
        };
        propagatedUserEnvPkgs = [ compat emacs xelb ];
        buildInputs = propagatedUserEnvPkgs;
      }
    ) { inherit (esuper) trivialBuild compat;
        inherit (eself)  xelb; };
  });
  emacsWithPackages = emacsPackages.emacsWithPackages;
  exwmOuterGaps = { trivialBuild, fetchFromGitHub, exwm, xelb, compat }:
    trivialBuild rec {
      pname = "exwm-outer-gaps";
      version = "20240108";
      src = fetchFromGitHub {
        owner = "cherrypiejam";
        repo = "exwm-outer-gaps";
        rev = "06af6089aaed6acedc59e20c5810047eac613251";
        hash = "sha256-9kEBF2kIrUOPooeWryr9o5yUXDQCAjlnFmYba1CeNzQ=";
      };
      propagatedUserEnvPkgs = [ exwm xelb compat ];
      buildInputs = propagatedUserEnvPkgs;
    };
  exwmFirefox = { trivialBuild, exwm, s, exwm-firefox-core, compat, xelb }:
    trivialBuild rec {
      pname = "exwm-firefox-core";
      version = "20240207";
      src = "${builtins.fetchGit {
        url = "https://codeberg.org/emacs-weirdware/exwm-firefox.git";
        ref = "main";
        rev = "ba4044cf57f99656bbe1974278336b6abcb15497";
        narHash = "sha256-eGeBEo6mzreYYA2TpLLQfstJ5pIoYRIO98/zokhiMPU=";
      }}/lisp";
      propagatedUserEnvPkgs = [ exwm exwm-firefox-core s compat xelb ];
      buildInputs = propagatedUserEnvPkgs;
    };
  # typstMode = { trivialBuild, writeText }:
  #   trivialBuild rec {
  #     pname = "typst-mode";
  #     version = "20240915";
  #     src = writeText "typst-mode.el" ''
  #       (provide 'typst-mode)
  #     '';
  #     propagatedUserEnvPkgs = [ ];
  #     buildInputs = propagatedUserEnvPkgs;
  #   };
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
    pdf-tools
    nix-mode
    rust-mode
    markdown-mode
    scala-mode
    evil-org
    pass
    password-store
    pyim
    pyim-basedict
    org-bullets
    org-mime
  ])
  ++ (with epkgs.melpaPackages; [
    vterm
  ])
  ++ (with epkgs.elpaPackages; [
    auctex
    org
    eglot
    company
    pinentry
  ])
  ++ (with epkgs; [
    tree-sitter
    (tree-sitter-langs.withPlugins (g: tree-sitter-langs.plugins ++ [
      g.tree-sitter-markdown
      # g.tree-sitter-typst
    ]))
    exwm
    (callPackage exwmOuterGaps {
      inherit trivialBuild exwm xelb compat;
    })
    (callPackage exwmFirefox {
      inherit trivialBuild exwm-firefox-core s exwm compat xelb;
    })
    # (callPackage typstMode {
    #   inherit trivialBuild;
    # })
  ])
  ++ [
    # pkgs.notmuch
  ])
