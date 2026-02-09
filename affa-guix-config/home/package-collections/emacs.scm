(define-module (affa-guix-config home package-collections emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)

  #:use-module (afistfullofash packages emacs-xyz)
  
  #:export (emacs-packages))

;; TODO: I think I can auto-generate this list
(define emacs-packages
  (list emacs-lucid

	;; Visual
	emacs-doom-themes
	emacs-doom-modeline
	emacs-nerd-icons
	emacs-nyan-mode
	emacs-rainbow-delimiters
	emacs-rainbow-mode
	emacs-catppuccin-theme
	emacs-darkman
	
	;; Org Mode
	emacs-org-modern
	emacs-org-modern-indent
        emacs-org-journal
	
	emacs-cape
	emacs-consult
	emacs-corfu
	emacs-dired-hacks
	emacs-diredfl
	emacs-dirvish
	emacs-expand-region
	emacs-format-all-the-code
	emacs-geiser-guile
	emacs-guix
	emacs-indent-bars
	emacs-jinx
	emacs-lsp-mode
	emacs-lsp-ui
	emacs-magit
	emacs-marginalia
	emacs-notmuch
	emacs-orderless
	emacs-prettier
	emacs-rustic
	emacs-sly
	emacs-smartparens
	emacs-syslog-mode
	emacs-terraform-mode
	emacs-undo-tree
	emacs-vertico
	emacs-web-mode
	emacs-yasnippet
        emacs-flycheck))
