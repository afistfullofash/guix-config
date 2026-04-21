(define-module (affa-guix-config systems package-collections base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages wm)

  #:use-module (nongnu packages mozilla)
  
  #:export (base-operating-system-packages))

(define bluetooth-packages
  (list blueman
	bluez
	bluez-alsa))

(define audio-packages
  (list alsa-plugins
	pamixer
	pipewire))

(define hardware-packages
  (list brillo
	i2c-tools
	openrgb
	;; lm-sensors and extras
	lm-sensors
	dmidecode))

(define font-packages
  (list font-google-noto
	font-liberation
	hicolor-icon-theme
	;; sbcl-stumpwm-ttf-fonts
	freetype
	fontconfig))

(define wm-packages
  (list pkg-config
	libfixposix
	sbcl-local-time
	waybar))

(define misc-packages
  (list bzip2
	firefox
	flatpak
	gzip
	simple-scan
	unzip
	xdg-utils
	zsh
	glibc-locales
	ntfs-3g))


(define base-operating-system-packages
  (append misc-packages
	  wm-packages
	  font-packages
	  bluetooth-packages
	  audio-packages
	  hardware-packages
	  font-packages))


