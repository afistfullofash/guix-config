(define-module (affa-guix-config home package-collections desktop)

  #:use-module (afistfullofash packages mail)
  #:use-module (afistfullofash packages rust-apps)

  #:use-module (gnu packages ebook)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages kde-graphics)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages music)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)

  #:use-module (nongnu packages game-client)
  #:use-module (nongnu packages mozilla)
  
  #:export (desktop-packages))

(define media-packages
  (list
   ;; Vlc
   vlc
   strawberry
   ;; Required for mtp mounting
   libmtp
   gvfs))

(define desktop-packages
  (append
   (list
    ;; Terminal
    alacritty
    ;; Password Management
    keepassxc
    ;; Email
    notmuch
    lieer
    ;; Documents
    libreoffice
    steam
    calibre
    pavucontrol
    ;; Screenshot tool
    maim
    ;; Image Viewer
    sxiv
    ;; Document Viewer
    zathura
    ;; Main browser
    firefox
    ;; Wallpaper Setter
    digikam-wallpaper)
   media-packages))

