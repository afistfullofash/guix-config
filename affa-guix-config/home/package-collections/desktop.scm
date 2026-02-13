(define-module (affa-guix-config home package-collections desktop)

  #:use-module (guix channels)
  #:use-module (guix inferior)
  
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

  #:use-module (srfi srfi-1)
  
  #:export (desktop-packages))

;; Calibre is a nasty little gremlin which had dependencys break the build often.
;; Pin it to a know good build
(define calibre-guix-channel
  (inferior-for-channels
   (list (channel
          (name 'guix)
          (url "https://codeberg.org/guix/guix.git")
          (commit "9c8e6a9b11c72c96fbdb6d8507338bcb483dd863")
	  (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))))

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
   (first (lookup-inferior-packages calibre-guix-channel "calibre"))
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

