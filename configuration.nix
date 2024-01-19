# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).

{ config, pkgs, lib, ... }: let
  nur-no-pkgs = import (builtins.fetchGit  {
    url = "https://github.com/nix-community/NUR";
    rev = "0f3c510de06615a8cf9a2ad3b77758bb9d155753";
    ref = "master";
  }) {};
  my-emacs = import /etc/nixos/emacs.nix { inherit pkgs; };
  tree-sitter-overlay = (self: super: {
    tree-sitter-grammars = super.tree-sitter-grammars // {
      tree-sitter-c = super.tree-sitter-grammars.tree-sitter-c.overrideAttrs (_: {
        nativeBuildInputs = [ self.nodejs self.tree-sitter ];
        configurePhase = ''
          tree-sitter generate --abi 13 src/grammar.json
        '';
      });
      tree-sitter-rust = super.tree-sitter-grammars.tree-sitter-rust.overrideAttrs (_: {
        nativeBuildInputs = [ self.nodejs self.tree-sitter ];
        configurePhase = ''
          tree-sitter generate --abi 13 src/grammar.json
        '';
      });
    };
  });
in {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs = {
    config.allowUnfreePredicate = pkg:
      builtins.elem (lib.getName pkg) [
        "dropbox"
        "dropbox-cli"
      ];
    overlays = [
      nur-no-pkgs.repos.cherrypiejam.overlays.wpa-supplicant-sslv3-trust-me
      tree-sitter-overlay
    ];
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostId = "9a8f4bf9";
  networking.hostName = "cheesecake";

  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.
  environment.etc."NetworkManager/system-connections" = {
    source = "/var/state/networkmanager-connections";
  };

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Set font packages.
  fonts.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "Hack" ]; })
  ];

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true; # use xkbOptions in tty.
  };

  # List services that you want to enable:
  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.dpi = 128;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "ctrl:swapcaps";
  services.xserver.desktopManager.gnome.enable = true;

  services.xserver.displayManager = {
    lightdm.enable = true;
    lightdm.background =
      "/var/state/background";
      # pkgs.nixos-artwork.wallpapers.nineish-dark-gray.gnomeFilePath;
    lightdm.greeters.gtk.extraConfig = ''
      hide-user-image=true
      [monitor: eDP-1]
      background=/var/state/background
    '';
    defaultSession = "none+exwm";
  };

  services.xserver.windowManager.session = lib.singleton {
    name = "exwm";
    start = ''
      for i in $(seq 100); do
        if [ "$(systemctl --user is-active emacs.service)" = "active" ]; then
          break
        fi
        sleep 0.1
      done
      ${my-emacs}/bin/emacsclient -c
    '';
  };

  services.xserver.libinput = {
    enable = true;
    touchpad.naturalScrolling = true;
    touchpad.tapping = true;
    touchpad.disableWhileTyping = true;
    touchpad.accelProfile = "flat";
    touchpad.accelSpeed = "0.9";
  };

  services.emacs = {
    enable = true;
    package = my-emacs;
    defaultEditor = true;
  };

  services.logind = {
    powerKey = "ignore";
    lidSwitch = "ignore";
  };

  services.acpid = {
    enable = true;
    lidEventCommands = ''
      lid=$(cat /proc/acpi/button/lid/LID0/state | ${pkgs.gawk}/bin/awk '{print $NF}')
      if [ "$lid" = "closed" ]; then
         echo "$(systemctl is-system-running)" > /dev/null
         systemctl suspend
      fi
    '';
    powerEventCommands = ''
      systemctl suspend
    '';
  };

  services.xserver.xautolock = {
    enable = true;
    time = 10;
    locker = "${pkgs.i3lock}/bin/i3lock";
    nowlocker = "${pkgs.i3lock}/bin/i3lock";
    extraOptions = [ "-corners 000-" ];
  };

  # Enable fingerprint reader
  # services.fprintd.enable = true;

  # Enable multi-display
  services.autorandr.enable = true;

  # Enable CUPS to print documents
  services.printing.enable = true;

  # Enable sound
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable bluetooth
  services.blueman.enable = true;
  hardware.bluetooth.enable = true;

  # Enable mail synchronizer
  services.offlineimap.enable = true;

  # Dropbox
  systemd.user.services.dropbox = {
    description = "Dropbox";
    wantedBy = [ "graphical-session.target" ];
    environment = {
      QT_PLUGIN_PATH = "/run/current-system/sw/" + pkgs.qt5.qtbase.qtPluginPrefix;
      QML2_IMPORT_PATH = "/run/current-system/sw/" + pkgs.qt5.qtbase.qtQmlPrefix;
    };
    serviceConfig = {
      ExecStart = "${lib.getBin pkgs.dropbox}/bin/dropbox";
      ExecReload = "${lib.getBin pkgs.coreutils}/bin/kill -HUP $MAINPID";
      KillMode = "control-group"; # upstream recommends process
      Restart = "on-failure";
      PrivateTmp = true;
      ProtectSystem = "full";
      Nice = 10;
    };
  };

  # Dictionary
  services.dictd = {
    enable = true;
    DBs = with pkgs.dictdDBs; [ wiktionary wordnet ];
  };

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = [
    my-emacs
  ] ++ (with pkgs; [
    vim
    wget
    tmux
    gitAndTools.gitFull
    gcc
    cmake
    gnumake
    libtool
    firefox
    file
    networkmanagerapplet
    arandr
    autorandr
    blueman
    pasystray
    brightnessctl
    pavucontrol
    acpi
    qemu
    dropbox-cli
    pass
    notmuch
    notmuch.emacs
    nil # nix language server
    rustup
    ccls
  ]);

  users.mutableUsers = false;
  users.users.root.hashedPassword =
    "";

  users.users.cherrypie = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "audio" "networkmanager" "tty" "dialout" "libvirtd" "tss" ];
    hashedPassword =
      "";
    shell = pkgs.fish;
    packages = with pkgs; [
      tree
      nyxt
      ispell
      htop
    ];
  };

  # Fall-back emacs daemon
  # systemd.user.services.emacs = {
  #   description = "Emacs: the extensible, self-documenting text editor";
  #   serviceConfig = {
  #     Type = "forking";
  #     ExecStart = "${pkgs.bash}/bin/bash -c 'source ${config.system.build.setEnvironment}; exec ${my-emacs}/bin/emacs --daemon'";
  #     ExecStop = "${my-emacs}/bin/emacsclient --eval (kill-emacs)";
  #     Restart = "always";
  #   };
  #   wantedBy = [ "default.target" ];
  # };

  systemd.user.services.emacs.serviceConfig = {
    Slice = "session.slice";
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.fish = {
    enable = true;
    shellAliases = {
      locknow = "xautolock -locknow";
      ed = "emacseditor";
    };
    shellInit = ''
    '';
  };
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    # enableSSHSupport = true;
  };

  # Enable firewall and open ports.
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 17500 ];
    allowedUDPPorts = [ 17500 ];
  };

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;
  system.extraSystemBuilderCmds = "ln -s ${./.} $out/nixos-config";

  # nix = {
  #   # gc = {
  #   #   automatic = true;
  #   #   options = "--delete-older-than 30d";
  #   # };
  #   extraOptions = ''
  #     experimental-features = nix-command flakes
  #   '';
  # };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}

