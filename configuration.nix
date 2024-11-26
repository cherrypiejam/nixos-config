# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).

{ config, pkgs, lib, ... }: let
  nur-no-pkgs = import (builtins.fetchGit  {
    url = "https://github.com/nix-community/NUR";
    rev = "0f3c510de06615a8cf9a2ad3b77758bb9d155753";
    ref = "master";
  }) {};
  tree-sitter-overlay = (self: super: {
    tree-sitter-grammars = super.tree-sitter-grammars // (builtins.listToAttrs (
      builtins.map (lang:
        let tree-sitter-lang = "tree-sitter-${lang}"; in
        lib.nameValuePair
          tree-sitter-lang
          (super.tree-sitter-grammars.${tree-sitter-lang}.overrideAttrs (_: {
            nativeBuildInputs = [ self.nodejs self.tree-sitter ];
            configurePhase = ''
                tree-sitter generate --abi 13 src/grammar.json
            '';
          }))
      ) [ "c" "rust" "scala" "python" ]
    ));
  });
  emacs-overlay = import (builtins.fetchGit {
    url = "https://github.com/nix-community/emacs-overlay";
    rev = "ab15d82f94c8f6373fec1f363c5b6c631453950b";
    ref = "master";
  });
  my-emacs = pkgs.callPackage ./emacs.nix {};
in {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./nix-individual-machines.nix
    ];

  nix = {
    distributedBuilds = true;
    buildMachines = [
      { hostName = "sns44";
        systems  = [ "x86_64-linux" "aarch64-linux" ];
        maxJobs  = 16;
        sshUser  = "cherrypiejam";
        sshKey   = "/home/cherrypie/.ssh/remote-build/builder";
        supportedFeatures = [ "big-parallel" ];
        speedFactor = 3;
      }
      { hostName = "mooncake-builder";
        systems  = [ "x86_64-linux" ];
        maxJobs  = 16;
        sshUser  = "nixremote";
        sshKey   = "/home/cherrypie/.ssh/remote-build/builder";
        supportedFeatures = [ "big-parallel" ];
        speedFactor = 1;
      }
    ];
  };

  # Local build by default
  # environment.etc."nix/machines".enable = true;

  nixpkgs = {
    config.allowUnfreePredicate = pkg:
      builtins.elem (lib.getName pkg) [ # Unfree software go here
        "corefonts"
        "hplip"
      ];
    overlays = [
      nur-no-pkgs.repos.cherrypiejam.overlays.wpa-supplicant-sslv3-trust-me
      tree-sitter-overlay
      # emacs-overlay
    ];
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Kernel version
  # boot.kernelPackages = pkgs.linuxPackages_6_8;

  # Cross-compile images
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

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
  # time.timeZone = "US/Central";

  # Set font packages.
  fonts = {
    packages = with pkgs; [
      (nerdfonts.override { fonts = [ "Hack" ]; })
      noto-fonts-cjk
      # corefonts
    ];
  };

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
  services.xserver.xkb.layout = "us";
  services.xserver.xkb.options = "ctrl:swapcaps";
  services.xserver.desktopManager.gnome.enable = true;

  services.xserver.displayManager = {
    lightdm.enable = true;
    lightdm.background =
      # "/var/state/background";
      pkgs.nixos-artwork.wallpapers.nineish-dark-gray.gnomeFilePath;
    lightdm.greeters.gtk.extraConfig = ''
      hide-user-image=true
    '';
    #   [monitor: eDP-1]
    #   background=/var/state/background
    # '';
  };

  services.displayManager.defaultSession = "none+exwm";

  services.xserver.windowManager.session = lib.singleton {
    name = "exwm";
    start = ''
      for i in $(seq 300); do
        if [ "$(systemctl --user is-active emacs.service)" = "active" ]; then
          break
        fi
        sleep 0.1
      done
      ${my-emacs}/bin/emacsclient -c
    '';
  };

  services.libinput = {
    enable = true;
    touchpad = {
      naturalScrolling = true;
      tapping = true;
      disableWhileTyping = true;
      accelProfile = "adaptive";
      accelSpeed = "0.7";
    };
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

  # Device firmware update
  services.fwupd.enable = true;

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
  services.fprintd.enable = false;

  # Enable multi-display
  services.autorandr.enable = true;

  # Enable CUPS to print documents
  services.printing = {
    enable = true;
    drivers = with pkgs; [ hplipWithPlugin ];
  };

  # CUPS security issues
  # https://discourse.nixos.org/t/cups-cups-filters-and-libppd-security-issues/52780
  services.avahi.enable = false;

  # Pulseaudio does not have great bluetooth support. Use Pipewire
  # instead to enable sound.
  sound.enable = false;
  hardware.pulseaudio.enable = false;
  # For user process realtime scheduling priority
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # Enable bluetooth
  services.blueman.enable = true;
  hardware.bluetooth = {
    enable = true;
    settings.General = {
      Enable = "Source,Sink,Media,Socket";
      Disable = "Headset";
    };
  };

  # Enable mail synchronizer
  services.offlineimap = {
    enable = true;
    path = with pkgs; [ bash notmuch ];
  };

  # Dictionary
  services.dictd = {
    enable = true;
    DBs = with pkgs.dictdDBs; [ wiktionary wordnet ];
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = false;

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
    gdb
    cmake
    gnumake
    libtool
    firefox
    file
    qemu
    pass
    gnutls
    notmuch
    notmuch.emacs
    zip
    unzip
    alsa-utils
    # Service essentials
    networkmanagerapplet
    arandr
    autorandr
    blueman
    pasystray
    brightnessctl
    pavucontrol
    acpi
    # Language-specifics
    nil
    rustup
    ccls
    scala
    metals
    (python3.withPackages (p: with p; [
      python-lsp-server
    ]))
    # Tools
    usbutils
    flameshot
    mplayer
  ]);

  users.mutableUsers = false;
  users.users.root.hashedPassword =
    "";

  users.users.cherrypie = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "audio" "networkmanager" "tty" "dialout" "libvirtd" "tss" "docker"
                    "lp" # HP printer
                    "usbmuxd"
                  ];
    hashedPassword =
      "";
    shell = pkgs.fish;
    packages = with pkgs; [
      tree
      nyxt
      ispell
      htop
      gtkwave
      texliveFull
    ];
  };

  # virtualisation.docker.enable = true;
  virtualisation.containers.enable = true;
  virtualisation.podman = {
    enable = true;
    # Drop-in replacement to docker
    dockerCompat = true;
    # Required for containers under podman-compose to be able to talk to each other
    defaultNetwork.settings.dns_enabled = true;
  };

  # Enable virtual box
  # virtualisation.virtualbox.host.enable = true;
  # virtualisation.virtualbox.host.enableExtensionPack = true; # Requires unfree
  # virtualisation.virtualbox.guest.enable = true;
  # virtualisation.virtualbox.guest.x11 = true;
  # users.extraGroups.vboxusers.members = [ "user-with-access-to-virtualbox" ];

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

  systemd.user.services.emacs = {
    serviceConfig = {
      Slice = "session.slice";
    };
    environment = {
      XMODIFIERS = "@im=exwm-xim";
      GTK_IM_MODULE = "xim";
      QT_IM_MODULE = "xim";
      CLUTTER_IM_MODULE = "xim";
    };
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.fish = {
    enable = true;
    shellAliases = {
      locknow = "xautolock -locknow";
      ed = "emacseditor";
      isnixshell = "echo $IN_NIX_SHELL";
    };
    shellInit = ''
    '';
  };

  programs.ssh = {
    extraConfig = ''
      Host mooncake-builder
           HostName mooncake.gongqi.zone
           User nixremote

      Host mooncake
           HostName mooncake.gongqi.zone
           User cherrypie

      Host sns44
           HostName sns44.cs.princeton.edu
           User cherrypiejam
    '';
  };

  programs.mtr.enable = true;

  programs.gnupg.agent = {
    enable = true;
    # enableSSHSupport = true;
  };

  programs.proxychains = {
    enable = true;
    package = pkgs.proxychains-ng;
    proxies = {
      v2ray = {
        enable = true;
        type = "socks5";
        host = "127.0.0.1";
        port = 1080;
      };
    };
  };

  # Enable firewall and open ports.
  networking.firewall = {
    enable = true;
    # allowedTCPPorts = [ 21 56260 ];
    # allowedUDPPorts = [ 8771 5353 ];
  };

  # Ifuse
  services.usbmuxd.enable = true;

  # Enable FTP server
  services.vsftpd = {
    enable = false;
    enableVirtualUsers = true;
    localUsers = true;
    localRoot = "/var/ftp";
    writeEnable = true;
    allowWriteableChroot = true;
    anonymousUploadEnable = true;
    extraConfig = ''
      pasv_enable=Yes
      pasv_min_port=56260
      pasv_max_port=56260
    '';
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
