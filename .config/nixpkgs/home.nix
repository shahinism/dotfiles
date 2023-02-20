{ config, pkgs, ... }:

{
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
  home.stateVersion = "22.11";

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
    
    htop
    emacs
    firefox
    brave
    slack
    git     # required for Emacs
    direnv  # required for `zplug: plugins/direnv`
    fzf
    gnupg

    ranger
    direnv

    yubikey-personalization
    yubikey-manager
    pcscliteWithPolkit
  ];

  programs.zsh = {
    enable = true;

    zplug = {
      enable = true;
      plugins = [
        {name = "plugins/git"; tags = [ from:oh-my-zsh ];}
        {name = "plugins/fzf"; tags = [ from:oh-my-zsh ];}
        {name = "plugins/fasd"; tags = [ from:oh-my-zsh ];}
        {name = "plugins/pip"; tags = [ from:oh-my-zsh ];}
        {name = "plugins/direnv"; tags = [ from:oh-my-zsh ];}
        {name = "plugins/command-not-found"; tags = [ from:oh-my-zsh ];}
        {name = "blimmer/zsh-aws-vault"; tags = [ at:main ];}
        {name = "zdharma-continuum/fast-syntax-highlighting";}
      ];
    };
    initExtra = "source ${./zsh/initExtra.zsh}";
  };

  programs.starship = {
    enable = true;
  };

  services.emacs.enable = true;
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "gtk2";
  };
}
