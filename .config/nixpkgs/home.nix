{ config, pkgs, ... }:

{
  imports = [
    ./firefox.nix
    ./zsh.nix
    ./git.nix
    ./dunst.nix
  ];
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "shahin";
  home.homeDirectory = "/home/shahin";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    gnumake
    cmake     # rquired by emacs to build vterm
    gcc

    # Required for Emacs vterm
    libvterm
    libtool

    unzip   # crucial for company-tabnine to unzip the package
    # otherwise you'll have an empty directory

    killall
    xorg.xkill
    
    htop
    emacs
    brave
    slack
    xclip
    fzf
    gnupg
    ripgrep

    kitty
    rofi
    espanso

    ranger
    direnv
    rpi-imager
    flameshot
    keybase-gui
    networkmanagerapplet
    blueman
    xss-lock

    pulseaudioFull
    obs-studio
    vlc
    gnome.gnome-tweaks
    
    yubikey-personalization
    yubikey-manager
    pcscliteWithPolkit
  ];

  home.shellAliases = {
    c = "xclip -selection clipboard";
    hm = "home-manager";
    man = "batman";
    watch = "batwatch";
    cat = "bat";
    grep = "batgrep";
    ls = "exa";
    mkdir = "mkdir -pv";
  };

  programs = {
    starship.enable = true;
    exa.enable = true;   # the ls replacement
    
    bat = {
      enable = true;
      extraPackages = with pkgs.bat-extras; [ batdiff batman batgrep batwatch ];
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
  };
  
  services.emacs.enable = true;
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "gtk2";
  };

  # Enable keybase requirements
  services.kbfs.enable = true;
  services.keybase.enable = true;
}
