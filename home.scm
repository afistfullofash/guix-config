(define-module (home)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu packages)
  #:use-module (gnu services)

  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix store)
  
  #:use-module (ice-9 popen)
  
  #:use-module (packages boundary)
  #:use-module (packages kubectl)
  #:use-module (packages terraform))

(define home-directory (getenv "HOME"))

(define dracula-gtk-theme-repo
  (origin
    (uri (git-reference
	  (url "https://github.com/dracula/gtk.git")
	  (commit "918905b1d6175c506ad0f56fa9bb799a912e45f1")))
    (method git-fetch)
    (sha256
     (base32
      "0411xwm0zaaajlviil2za14113jlgksxayxvaxp73rrq8mi8x63i"))))

(define dracula-alacritty-theme-repo
  (origin
    (uri (git-reference
	  (url "https://github.com/dracula/alacritty.git")
	  (commit "c8a3a13404b78d520d04354e133b5075d9b785e1")))
    (method git-fetch)
    (sha256
     (base32
      "1pmk7m2bcwmnmhrbvnnm2znmyyh3vp42vvl1phvfbkz5yqz5jf2b"))))

(define dracula-starship-theme-repo
  (origin
    (uri (git-reference
	  (url "https://github.com/dracula/starship.git")
	  (commit "bd1383b8a078d7d6829fa71a8e7c4119ae77e962")))
    (method git-fetch)
    (sha256
     (base32
      "0kk8cjbz004xjk3lm1w233p8qp7nrymqm803f9474df7sr2574av"))))

(define dracula-xresources-theme-repo
  (origin
    (uri (git-reference
	  (url "https://github.com/dracula/xresources.git")
	  (commit "539ef24e9b0c5498a82d59bfa2bad9b618d832a3")))
    (method git-fetch)
    (sha256
     (base32
      "1dkfa2q392vy7ky5kx0vd44xcb9c7x15z38x4acfma3f16q6vyg9"))))

(define nnn-plugins-repo
  (origin
    (uri (git-reference
	  (url "https://github.com/jarun/nnn.git")
	  (commit "v5.1")))
    (method git-fetch)
    (sha256
     (base32
      "17mygdwdsb49zd0w1r4injaybmwp99dhhaabj30i7aas3ca4asgv"))))


(define-public default  
  (home-environment
   (packages (specifications->packages
	      (list "alacritty"
		    "awscliv2"
		    "bottom"
		    "boundary"
		    "dex"
		    "emacs"
		    "feh"
		    "font-dejavu"
		    "glibc-locales"
		    "gnupg"
		    "gawk"
		    "git"
		    "git:credential-libsecret"
		    "git-lfs"
		    "jq"
		    "keepassxc"
		    "kubectl"
		    "make"
		    "mpv"
		    "nnn"
		    "obsidian"
		    "openssh"
		    "protonup-ng"
		    "ripgrep"
		    "scrot"
		    "skim"
		    "starship"
		    "steam"
		    "sxiv"
		    "tabbed"
		    "terraform"
		    "zathura"
		    "zsh-autosuggestions"
		    "zsh-history-substring-search"
		    "zsh-syntax-highlighting")))
   (services
    (list (service home-dbus-service-type)
	  (service home-pipewire-service-type)
	  (service home-syncthing-service-type)
	  (service home-redshift-service-type)
(service home-openssh-service-type
         (home-openssh-configuration
          (hosts
           (list (openssh-host (name "*")
                               (identity-file "~/.ssh/tom.pub"))
                 (openssh-host (name "gitlab.com")
                               (identity-file "~/.ssh/work.pub"))))))
(service home-ssh-agent-service-type)
	  (service home-files-service-type
		   `((".themes/Dracula" ,dracula-gtk-theme-repo)
		     (".Xresources" ,dracula-xresources-theme-repo)
		     (".gitconfig" ,(local-file "files/git/gitconfig"))
		     (".gitignore" ,(local-file "files/git/gitignore"))
		     ("work/.gitconfig" ,(local-file "files/git/work.gitconfig"))
		     (".emacs.d/init.el" ,(local-file "files/emacs/init.el"))
		     ;; For some reason this does not work when we pass directories to it
		     (".stumpwm.d/init.lisp" ,(local-file "files/stumpwm/init.lisp"))
		     (".stumpwm.d/keybindings.lisp" ,(local-file "files/stumpwm/keybindings.lisp"))
		     (".stumpwm.d/visual.lisp" ,(local-file "files/stumpwm/visual.lisp"))
		     (".ssh/tom.pub" ,(local-file "files/ssh/tom.pub"))
		     (".ssh/work.pub" ,(local-file "files/ssh/work.pub"))
		     ;; Ensure screenshot directory exists
		     ("Pictures/Screenshots/.keep" ,(local-file "files/keep"))))
	  (service home-xdg-configuration-files-service-type
		   `(("assets" ,(symlink-to "$HOME/.themes/Dracula/assets"))
		     ("gtk-4.0/gtk.css" ,(symlink-to "$HOME/.themes/Dracula/gtk-4.0/gtk.css"))
		     ("gtk-4.0/gtk-dark.css" ,(symlink-to "$HOME/.themes/Dracula/gtk-4.0/gtk-dark.css"))
		     ("alacritty/alacritty.toml" ,(local-file "files/alacritty/alacritty.toml"))
		     ("alacritty/themes/dracula" ,dracula-alacritty-theme-repo)
		     ("nnn/plugins" ,nnn-plugins-repo)
		     ("starship.toml" ,(file-append dracula-starship-theme-repo "/starship.theme.toml"))
		     ("autostart/keepassxc.desktop" ,(local-file "files/autostart/keepassxc.desktop"))))
	  (simple-service 'some-useful-env-vars-service
			  home-environment-variables-service-type
			  `(("GUIX_HOME_PATH" . "$HOME/.guix-home/profile")
			    ("PATH" . ,(string-join '("${HOME}/.cargo/bin/" ; Cargo
						      "${KREW_ROOT:-$HOME/.krew}/bin" ; Krew
						      "${HOME}/src/shell-scripts/" ; Custom Shell Scripts
						      "${HOME}/.local/bin" ; Locally Installed Binarys
						      "${PATH}") ; Original Value
						    ":"))
			    ("GUIX_SANDBOX_EXTRA_SHARES" . "/steam")
			    ("BOUNDARY_KEYRING_TYPE" . "secret-service")
			    ("NNN_FIFO" . "/tmp/nnn.fifo")
			    ("TZ" . "Australia/Sydney")))
	  (simple-service 'variant-packages-service
			  home-channels-service-type
			  (list
			   (channel
			    (name 'nonguix)
			    (url "https://gitlab.com/nonguix/nonguix")
			    ;; Enable signature verification:
			    (introduction
			     (make-channel-introduction
			      "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
			      (openpgp-fingerprint
			       "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))))

	  (service home-zsh-service-type
		   (home-zsh-configuration
		    (zshrc (list
			    (local-file "files/zsh/zshrc.sh")))))))))

default
