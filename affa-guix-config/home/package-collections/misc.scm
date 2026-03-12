(define-module (affa-guix-config home package-collections misc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages sync)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  
  #:use-module (nongnu packages game-client)
  
  #:use-module (afistfullofash packages fonts)
  #:use-module (affa-guix-config packages guix-reconfiguration-wrapper)
  
  #:export (misc-packages))

(define misc-packages
  (list fonts-nerd-fonts-dejavu
	font-dejavu
	protonup
	gnupg
	wireplumber
	;; Required by dirvish
	vips
	(list glib "bin")
	poppler
	mediainfo
	openssh
	;; Background Setter
	feh
	tabbed
	;; Runs autorun files
	dex
	;; Gives us dunstify for a more powerfull notify-send replacement
	dunst
        glibc-locales
	rclone
	;; xdg-desktop-portals
	xdg-desktop-portal
	xdg-desktop-portal-gtk

	xrdb
	xsetroot
	
	guix-reconfiguration-wrapper))
