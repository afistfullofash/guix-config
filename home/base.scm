(define-module (home base)
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

  #:use-module (themes dracula)
  
  #:use-module (afistfullofash packages boundary)
  ;; #:use-module (afistfullofash packages runst)
  #:use-module (afistfullofash packages codex)
  #:use-module (afistfullofash packages kubectl)
  #:use-module (afistfullofash packages emacs-xyz)
  #:use-module (afistfullofash packages fonts)
  #:use-module (afistfullofash packages terraform)

  #:export (base-home-environment))

(define home-directory (getenv "HOME"))


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
	"pre-commit"
	"awscliv2"
	"boundary"
	"gawk"
	"jq"
	"kubectl"
	"terraform"))

;;; This tangles the emacs init org file into init.el
;;; Ensuring that the emacs config is never out of date
(define emacs-init-el
  (computed-file
   "init.el"
   (with-imported-modules '((guix build utils))
     #~(begin
	 (use-modules (guix build utils))
	 (let* ((emacs-bin (string-append #$(file-append (specification->package "emacs") "/bin/emacs")))
		(init-org-file #$(local-file "files/emacs/init.org")))
	   (format #t "Tangling ~a...\n" init-org-file)
	   (invoke emacs-bin
		   "--batch"
		   "-q"	; Don't load a user init file
		   init-org-file ; Open the org file in a buffer
		   "--eval" "(require 'org)"
		   "--eval" "(org-babel-tangle)"))
	 (rename-file (string-append (dirname #$(local-file "files/emacs/init.org")) "/init.el")
		      #$output)))))

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
	"emacs-corfu"
	"emacs-sly"))

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
 ;; "calibre"
 "pavucontrol"
 ;; Backup file manager
 "thunar"
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
      "wireplumber"
      ;; Required by dirvish
      "vips"
      "poppler"
      "mediainfo"
      "openssh"
      ;; Background Setter
      "feh"
      ;; "runst"
      "tabbed"
      ;; Runs autorun files
      "dex"
      "glibc-locales"))


;; (define runst-service
;;   (simple-service
;;    'runst home-shepherd-service-type
;;    (list
;;     (shepherd-service
;;      (documentation "Run the runst notification daemon")
;;      (requirement '(x11-display))
;;      (auto-start? #t)
;;      (provision '(runst))
;;      (start #~(make-forkexec-constructor
;;                (list #$(file-append runst "/bin/runst"))))
;;      (stop #~(make-kill-destructor))))))

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
		  ("GUILE_LOAD_PATH" . "~/src/guix-config:~/src/afistfullofash")
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

(define stumpwm-init-lisp
  (computed-file
   "init.lisp"
   (with-imported-modules '((guix build utils))
     #~(begin
	 (use-modules (guix build utils))
	 (let* ((emacs-bin (string-append #$(file-append (specification->package "emacs") "/bin/emacs")))
		(init-org-file #$(local-file "files/stumpwm/config.org")))
	   (format #t "Tangling ~a...\n" init-org-file)
	   (invoke emacs-bin
		   "--batch"
		   "-q"	; Don't load a user init file
		   init-org-file ; Open the org file in a buffer
		   "--eval" "(require 'org)"
		   "--eval" "(org-babel-tangle)"))
	 (rename-file (string-append (dirname #$(local-file "files/stumpwm/config.org")) "/init.lisp")
		      #$output)))))


(define home-file-locations
  `((".themes/Dracula" ,dracula-gtk-theme-repo)
    (".icons/Dracula" ,dracula-gtk-icons)
    (".Xresources" ,dracula-xresources-theme-repo)
    (".gitconfig" ,(local-file "files/git/gitconfig"))
    (".gitignore" ,(local-file "files/git/gitignore"))
    ("work/.gitconfig" ,(local-file "files/git/work.gitconfig"))
    ;; For some reason this does not work when we pass directories to it
    (".stumpwm.d/init.lisp" ,stumpwm-init-lisp)
    ;; (".stumpwm.d/keybindings.lisp" ,(local-file "files/stumpwm/keybindings.lisp"))
    ;; (".stumpwm.d/visual.lisp" ,(local-file "files/stumpwm/visual.lisp"))
    (".ssh/tom.pub" ,(local-file "files/ssh/tom.pub"))
    (".ssh/work.pub" ,(local-file "files/ssh/work.pub"))
    (".emacs.d/init.el" ,emacs-init-el)
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

(define base-home-environment
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

   ;; runst-service
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
