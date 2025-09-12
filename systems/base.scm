(define-module (systems base)
  #:use-module (guix utils)
  #:use-module (guix packages)

  #:use-module (gnu)

  #:use-module (gnu services)
  #:use-module (gnu services linux)
  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (gnu services docker)

  
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages texinfo)
  
  #:use-module (gnu packages shells)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages hardware)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:use-module (srfi srfi-1)

  #:export (base-operating-system)
  #:export (stumpwm-with-extensions))

(define stumpwm-with-extensions
  (package
    (inherit stumpwm)
    (name "stumpwm-with-extensions")
    (inputs
     (list stumpwm
           sbcl-local-time
           sbcl-stumpwm-battery-portable
           sbcl-stumpwm-notify
           sbcl-stumpwm-pamixer
           sbcl-slynk))
    (native-inputs
     (list pkg-config
           gcc-toolchain
	   autoconf
	   texinfo))
    (arguments
     (substitute-keyword-arguments (package-arguments stumpwm)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (program (string-append out "/bin/stumpwm")))
                 ;; Avoid SBCL poking homedir during image build
                 (setenv "HOME" "/tmp")
                 (build-program program outputs
                   ;; Start stumpwm normally
                   #:entry-program '((stumpwm:stumpwm) 0)
                   ;; ASDF systems to pre-bundle
                   #:dependencies '("stumpwm"
                                    "local-time"
                                    "battery-portable"
                                    "notify"
                                    "pamixer"
                                    "slynk")
                   ;; Where to find those systems
                   #:dependency-prefixes
                   (map (lambda (input) (assoc-ref inputs input))
                        '("stumpwm"
                          "sbcl-local-time"
                          "sbcl-stumpwm-battery-portable"
                          "sbcl-stumpwm-notify"
                          "sbcl-stumpwm-pamixer"
                          "sbcl-slynk"))))))
           (delete 'copy-source)
           (delete 'build)
           (delete 'check)
           (delete 'remove-temporary-cache)
           (delete 'cleanup)))))))

(define etc-sudoers-config
  (plain-file "etc-sudoers-config"
              "Defaults  timestamp_timeout=480
root      ALL=(ALL) ALL
%wheel    ALL=(ALL) ALL
natalie  ALL=(ALL) NOPASSWD:/run/current-system/profile/sbin/reboot,/run/current-system/profile/sbin/shutdown,/home/natalie/.config/guix/current/bin/guix,/run/current-system/profile/bin/brillo"))

(define base-operating-system
  (operating-system
    (host-name "base")
    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))
    (locale "en_AU.utf8")
    (timezone "Australia/Sydney")
    (keyboard-layout (keyboard-layout "au"))


    (users (cons* (user-account
		   (name "natalie")
		   (comment "Natalie Atkinson")
		   (group "users")
		   (home-directory "/home/natalie")
		   (shell (file-append zsh "/bin/zsh"))
		   (supplementary-groups '("wheel" "netdev" "audio" "video" "docker" "lp")))
		  %base-user-accounts))

  (sudoers-file etc-sudoers-config)
    
    (packages (append
	       (specifications->packages
		'("blueman"
		  "bluez"
		  "bluez-alsa"
		  ;; Backlight and LED Control
		  "brillo"
		  "glibc-locales"
		  "alsa-plugins"
		  "xdg-utils"
		  "firefox"
		  "zsh"
		  "font-google-noto"
		  "font-liberation"
		  "hicolor-icon-theme"
		  "i2c-tools"
		  "openrgb"
		  "flatpak"
		  "gzip"
		  "bzip2"
		  "unzip"
		  "simple-scan"
		  "pamixer"
		  ;; sbcl-stumpwm-notify wanted these
		  "pkg-config"
		  "libfixposix"
		  ;; For setting the screenshot time
		  "sbcl-local-time"))
	       (list stumpwm-with-extensions)
	       %base-packages))

    (services
     (append (list
	      (service kernel-module-loader-service-type
		       '("i2c-dev" "i2c-piix4"))
	      (udev-rules-service 'openrgb openrgb)
	      (service bluetooth-service-type
		       (bluetooth-configuration (auto-enable? #t)
						(multi-profile 'multiple)))	    
	      (service iwd-service-type)
	      (service connman-service-type
		       (connman-configuration
			(disable-vpn? #t)
			(shepherd-requirement '(iwd))))
	      (service containerd-service-type)
	      (service docker-service-type)
	      (service cups-service-type
		       (cups-configuration
			(web-interface? #t)))
	      (service gnome-keyring-service-type)
	      (set-xorg-configuration
	       (xorg-configuration (keyboard-layout keyboard-layout))))
	     (modify-services %desktop-services
	       (delete wpa-supplicant-service-type)
	       (delete network-manager-service-type)
	       (guix-service-type config => (guix-configuration
					     (inherit config)
					     (substitute-urls
					      (append (list "https://substitutes.nonguix.org")
						      %default-substitute-urls))
					     (authorized-keys
					      (append (list (local-file "files/nonguix/signing-key.pub"))
						      %default-authorized-guix-keys)))))))
    
    (file-systems (cons*
                   (file-system
                     (mount-point "/tmp")
                     (device "none")
                     (type "tmpfs")
                     (check? #f))
                   %base-file-systems))
		      
    (bootloader (bootloader-configuration
		 (bootloader grub-efi-bootloader)
		 (targets (list "/boot/efi"))
		 (keyboard-layout keyboard-layout)))
    (name-service-switch %mdns-host-lookup-nss)))
