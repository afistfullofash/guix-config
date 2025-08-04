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
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)

  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix utils)
  
  #:use-module (ice-9 popen)
  
  #:use-module (afistfullofash packages boundary)
  #:use-module (afistfullofash packages runst)
  #:use-module (afistfullofash packages codex)
  #:use-module (afistfullofash packages kubectl)
  #:use-module (afistfullofash packages emacs-xyz)
  #:use-module (afistfullofash packages fonts)
  #:use-module (afistfullofash packages terraform)
  #:use-module (afistfullofash packages tree-sitter))

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

(define dracula-qt5-theme-repo
  (origin
    (uri (git-reference
	  (url "https://github.com/dracula/qt5.git")
	  (commit "7b25ee305365f6e62efb2c7aca3b4635622b778c")))
    (method git-fetch)
    (sha256
     (base32
      "00qlajbxj25w1bdhj8wc5r57g25gas6f1ax6wrzb4xcypw0j7xdm"))))

(define dracula-gtk-icons
  (origin
    (uri (git-reference
	  (url "https://github.com/m4thewz/dracula-icons.git")
	  (commit "6232e5217429a3ae6396c9e054f5338cecdbb7a5")))
    (method git-fetch)
    (sha256
     (base32
      "0n6mh6zypi5nhwl2wr20w77fcjfg682n8fm848awyqnn698hhrxv"))))

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
	  (url "https://github.com/tpine/starship.git")
	  (commit "b9c82888b28d232a2ea2dd9ec03cca893834e761")))
    (method git-fetch)
    (sha256
     (base32
      "13i6alr7djb9h3vzav199i2kkxmzn004815z5cbc41lf7xvx2nc0"))))

(define dracula-lsd-theme-repo
  (origin
    (uri (git-reference
	  (url "https://github.com/dracula/lsd.git")
	  (commit "2b87711bdce8c89a882db720e4f47d95877f83a7")))
    (method git-fetch)
    (sha256
     (base32
      "10id0n5c9jyrah295dv2zahl97851kp24d513k3pyxbsy9nv0qml"))))

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


(define terminal-packages
  (list
   ;; Top Replacement
   "bottom"
   ;; ls replacement
   "lsd"
   ;; cat replacement
   "bat"
   ;; ps replacement
   "procs"
   ;; fuzzy finding
   "fzf"
   "fzf-tab"
   ;; file manager (probably remove) 
   "nnn"
   ;; grep replacement
   "ripgrep"
   "mpv"
   ;; prompt replacement
   "starship"
   "codex"
   "unzip"))

(define development-packages
  (list "git"
	"git:credential-libsecret"
	"git-lfs"
	"awscliv2"
	"boundary"
	"gawk"
	"jq"
	"kubectl"
	"terraform"))

(define emacs-packages
  (list "emacs-doom-themes"
	"emacs-nyan-mode-1.1.4"
	"emacs-expand-region"
	"emacs-dirvish"
	"emacs-rainbow-delimiters"
	"emacs-guix"
	"emacs-geiser"
	"emacs-geiser-guile"
	"emacs-indent-bars"
	"emacs-dired-hacks"
	"emacs-undo-tree"
	"emacs-paredit"
	"emacs-smartparens"
	"emacs-magit"
	"emacs-rustic"
	"emacs-terraform-mode"
	"emacs-web-mode"
	"emacs-prettier"
	"emacs-lsp-mode"
	"emacs-lsp-ui"
	"emacs-lsp-scheme"
	"emacs-flycheck"
	"emacs-vertico"
	"emacs-orderless"
	"emacs-marginalia"
	"emacs-consult"
	"emacs-cape"
	"emacs-corfu"))

(define tree-sitter-grammars
  (list tree-sitter-typescript
	tree-sitter-scheme
	tree-sitter-rust
	tree-sitter-ruby
	tree-sitter-python
	tree-sitter-php
	tree-sitter-org
	tree-sitter-nix
	tree-sitter-meson
	tree-sitter-markdown
	tree-sitter-json
	tree-sitter-javascript
	tree-sitter-html
	tree-sitter-hcl
	tree-sitter-gomod
	tree-sitter-yaml
	tree-sitter-go
	tree-sitter-dockerfile
	tree-sitter-css
	tree-sitter-cmake
	tree-sitter-c-sharp
	tree-sitter-bash))

(define language-server-packages
  (list "rust-analyzer"
	"python-lsp-server"
	"guile-lsp-server"
	"sqls"))

(define desktop-packages
  (list
   ;; Terminal
   "alacritty"
   "emacs"
   ;; Password Management
   "keepassxc"
   ;; Notetaking Tool
   "obsidian"
   "steam"
   ;; Main browser
   "firefox"
   ;; Backup if firefox fails
   "google-chrome-stable"
   "calibre"
   ;; Screenshot tool
   "maim"
   ;; Image Viewer
   "sxiv"
   ;; Document Viewer
   "zathura"))

(define zsh-plugins
  (list "zsh-autosuggestions"
	"zsh-completions"
	"zsh-history-substring-search"
	"zsh-syntax-highlighting"))

(define misc-packages
  (list "fonts-nerd-fonts-dejavu"
	"font-dejavu"
	"autorandr"
	"protonup-ng"
	"gnupg"
	;; Required by dirvish
	"vips"
	"poppler"
	"mediainfo"
	"openssh"
	;; Background Setter
	"feh"
	"runst"
	"tabbed"
	;; Runs autorun files
	"dex"
	"glibc-locales"))


(define runst-service
  (simple-service
   'runst home-shepherd-service-type
   (list
    (shepherd-service
     (documentation "Run the runst notification daemon")
     (requirement '(x11-display))
     (auto-start? #t)
     (provision '(runst))
     (start #~(make-forkexec-constructor
               (list #$(file-append runst "/bin/runst"))))
     (stop #~(make-kill-destructor))))))

(define autorandr-service
  (simple-service
   'autorandr home-shepherd-service-type
   (list
    (shepherd-service
     (documentation "Run autorandr to set screens")
     (requirement '(x11-display))
     (auto-start? #t)
     (one-shot? #t)
     (provision '(autorandr))
     (start #~(make-forkexec-constructor
               (list #$(file-append autorandr "/bin/autorandr") "--change")))
     (stop #~(make-kill-destructor))))))

(define variant-packages-service
  (simple-service 'variant-packages-service
		  home-channels-service-type
		  (list
		   (channel
		    (name 'afistfullofash)
		    (url "https://github.com/afistfullofash/afistfullofash")
		    (branch "main"))
		   (channel
		    (name 'rustup)
		    (url "https://github.com/declantsien/guix-rustup")
		    (introduction
		     (make-channel-introduction
		      "325d3e2859d482c16da21eb07f2c6ff9c6c72a80"
		      (openpgp-fingerprint
		       "F695 F39E C625 E081 33B5  759F 0FC6 8703 75EF E2F5"))))
		   (channel
		    (name 'nonguix)
		    (url "https://gitlab.com/nonguix/nonguix")
		    ;; Enable signature verification:
		    (introduction
		     (make-channel-introduction
		      "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
		      (openpgp-fingerprint
		       "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))))

(define environment-variables-service
  (simple-service 'some-useful-env-vars-service
		  home-environment-variables-service-type
		  `(("GTK_THEME" .  "Dracula")
		    ("GUIX_HOME_PATH" . ,(string-append home-directory
							"/.guix-home/profile"))
		    ("PATH" . ,(string-join (list (string-append home-directory
								 "/src/shell-scripts/") ; Custom Shell Scripts
						  "${PATH}") ; Original Value
					    ":"))
		    ("GUIX_SANDBOX_EXTRA_SHARES" . "/steam" )
		    ("BOUNDARY_KEYRING_TYPE" . "secret-service")
		    ("BAT_THEME" . "Dracula")
		    ("NNN_FIFO" . "/tmp/nnn.fifo")
		    ("TREE_SITTER_LIBDIR"
		     . ,#~(string-join (map (lambda (pkg)
					      (string-append pkg "/lib"))
					    '#$tree-sitter-grammars)
				       ":"))
		    ;; Locale
		    ("TZ" . "Australia/Sydney")
		    ("LC_ALL" . "en_AU.utf8")
		    ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share"))))

(define home-file-locations
  `((".themes/Dracula" ,dracula-gtk-theme-repo)
    (".icons/Dracula" ,dracula-gtk-icons)
    (".Xresources" ,dracula-xresources-theme-repo)
    (".gitconfig" ,(local-file "files/git/gitconfig"))
    (".gitignore" ,(local-file "files/git/gitignore"))
    ("work/.gitconfig" ,(local-file "files/git/work.gitconfig"))
    ;; For some reason this does not work when we pass directories to it
    (".stumpwm.d/init.lisp" ,(local-file "files/stumpwm/init.lisp"))
    (".stumpwm.d/keybindings.lisp" ,(local-file "files/stumpwm/keybindings.lisp"))
    (".stumpwm.d/visual.lisp" ,(local-file "files/stumpwm/visual.lisp"))
    (".ssh/tom.pub" ,(local-file "files/ssh/tom.pub"))
    (".ssh/work.pub" ,(local-file "files/ssh/work.pub"))
    (".emacs.d/init.el" ,(local-file "files/emacs/init.el"))
    ;; Ensure screenshot directory exists
    ("Pictures/Screenshots/.keep" ,(local-file "files/keep"))))

(define xdg-config-file-locations
  ;; This Stats with a heap of THEMEING
  `(("assets" ,(symlink-to "$HOME/.themes/Dracula/assets"))
    ;; GTK
    ("gtk-4.0/gtk.css" ,(symlink-to "$HOME/.themes/Dracula/gtk-4.0/gtk.css"))
    ("gtk-4.0/gtk-dark.css" ,(symlink-to "$HOME/.themes/Dracula/gtk-4.0/gtk-dark.css"))
    ("gtk-4.0/settings.ini" ,(local-file "files/gtk-4.0/settings.ini"))
    ;; QT5
    ("qt5ct/colors" ,dracula-qt5-theme-repo)
    ("qt5ct/qt5ct.conf" ,(local-file "files/qt5ct/qt5ct.conf"))
    ;; Specific Programs
    ("alacritty/alacritty.toml" ,(local-file "files/alacritty/alacritty.toml"))
    ("alacritty/themes/dracula" ,dracula-alacritty-theme-repo)
    ("lsd" ,dracula-lsd-theme-repo)
    ("nnn/plugins" ,nnn-plugins-repo)
    ("starship.toml" ,(file-append dracula-starship-theme-repo "/starship.theme.toml"))
    ("autorandr" ,(local-file "files/autorandr"
			      #:recursive? #t))
    ("runst/runst.toml" ,(local-file "files/runst/runst.toml"))

    ;; Autostarts
    ("autostart/keepassxc.desktop" ,(local-file "files/autostart/keepassxc.desktop"))))

(define ssh-configuration
  (home-openssh-configuration
   (hosts
    (list (openssh-host (name "*")
			(identity-file "~/.ssh/tom.pub"))
	  (openssh-host (name "gitlab.com")
			(identity-file "~/.ssh/work.pub"))))))

(define-public default  
  (home-environment
   (packages (append (specifications->packages
		      (append terminal-packages
			      development-packages
			      desktop-packages
			      emacs-packages
			      language-server-packages
			      zsh-plugins
			      misc-packages))
		     tree-sitter-grammars))
   (services
    (list

     runst-service
     autorandr-service
     environment-variables-service
     variant-packages-service

     (service home-dbus-service-type)
     (service home-pipewire-service-type)
     (service home-syncthing-service-type)
     (service home-redshift-service-type)
     (service home-ssh-agent-service-type)
     (service home-openssh-service-type ssh-configuration)
     (service home-files-service-type home-file-locations)
     (service home-xdg-configuration-files-service-type xdg-config-file-locations)
     (service home-zsh-service-type
	      (home-zsh-configuration
	       (zshrc (list
		       (local-file "files/zsh/zshrc.sh")))))))))

default
