{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  # boot options
  boot = {
    kernelPackages = pkgs.linuxPackages_3_18;
    # msr is for powertop
    kernelModules = [ "kvm-intel" "tp_smapi" "msr" ];
    extraModulePackages = [ config.boot.kernelPackages.tp_smapi ];
    extraModprobeConfig = ''
      options iwlwifi 11n_disable=1 power_save=0
      options sdhci debug_quirks=0x4670
      options thinkpad_acpi fan_control=1
    '';

    blacklistedKernelModules = [ "snd_pcsp" "pcspkr" ];

    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
    };

  };
  nixpkgs.config.allowUnfree = true;

  networking = {
    hostName = "alzhared";
    hostId = "ab0aa677";
    # wireless.enable = false;
    enableIPv6 = false;
    useDHCP = true;
    networkmanager.enable = true;
  };

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  environment.systemPackages = with pkgs; [
    wget
    curl
    mosh
    firefoxWrapper
    emacs
    zsh
    gitAndTools.gitFull
    gitAndTools.tig
    networkmanagerapplet
    stalonetray
    dmenu
    haskellPackages.xmobar
    ubuntu_font_family
    chromium
    skype
    xlibs.xev
    xlibs.xinput
    xlibs.xmessage
    xlibs.xmodmap
    texLiveFull
    gnome3.gnome-screenshot
    gnumake
    python27Packages.pygments
    silver-searcher
    evince
    pavucontrol
  ];

  nixpkgs.config.chromium = {
    enablePepperFlash = true;
    enablePepperPDF = true;
  };

  # List services that you want to enable:
  services = {
    # Enable the OpenSSH daemon.
    # openssh.enable = true;

    # Enable CUPS to print documents.
    # printing.enable = true;

    locate.enable = true;
    # thinkfan.enable = true;
    # thinkfan.sensor = "/sys/class/hwmon/hwmon1/temp1_input";
  };

  services.xserver = {  
    # Enable the X11 windowing system.
    enable = true;
    layout = "us";
    # services.xserver.xkbOptions = "eurosign:e";

    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    windowManager.xmonad.extraPackages = self: [ self.xmonadContrib ];

    displayManager.lightdm.enable = true;
    windowManager.default = "xmonad";
    desktopManager.xfce.enable = true;

    # Enable the KDE Desktop Environment.
    # services.xserver.desktopManager.kde4.enable = true;
  };

  hardware = {
    enableAllFirmware = true;
    trackpoint.emulateWheel = true;
    pulseaudio.enable = true;
    bluetooth.enable = true;
  };

  time.timeZone = "America/Chicago";
}
