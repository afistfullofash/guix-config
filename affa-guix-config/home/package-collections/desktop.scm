(define-module (affa-guix-config home package-collections desktop)

  #:use-module (afistfullofash packages mail)

  #:use-module (gnu packages ebook)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages kde-graphics)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages mail)
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

(define desktop-packages
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
   ;; Vlc
   vlc
   calibre
   pavucontrol
   ;; Screenshot tool
   maim
   ;; Image Viewer
   sxiv
   ;; Photo Management
   darktable
   digikam
   rawtherapee
   ;; Document Viewer
   zathura
   ;; Main browser
   firefox))

