;;; Commentary:
;;; Entry Point for Emacs customization

;;; Code:

;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))
;; Add Line numbers onto a sidebar
(global-display-line-numbers-mode)

;; Backup File Directory
(let* ((home-dir (getenv "HOME"))
      (backup-dir "~/.tmp/emacs/backups")
      (auto-saves-dir "~/.tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (file-directory-p dir)
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defun internet-up-p (&optional host)
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1" 
		     (if host host "www.google.com"))))

;;; Themeing
(use-package doom-themes
  :ensure t
  :init
  (progn 
    (require 'doom-themes)

    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	  doom-themes-enable-italic t
	  doom-vibrant-brighter-modeline nil
	  org-hide-leading-stars nil) ; if nil, italics is universally disabled
    (load-theme 'doom-dracula t)
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)))

(use-package nyan-mode
  :ensure t
  :init
  ;; Fix up Nyan Cat cause she's pretty
  (setq nyan-animate-nyancat t
	nyan-wavy-trail t)
  (nyan-mode))


(use-package dirvish
  :ensure t
  :init (dirvish-override-dired-mode) ; swaps Dired transparently
  :custom
  (dirvish-preview-enabled t)   ; live previews
  (dirvish-use-header-line t))

;;; Rainbow Delimeters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package expand-region
  :ensure t                          ;; auto-install from MELPA/ELPA
  :bind (("C-=" . er/expand-region)  ;; grow region (like many IDEs)
         ("C--" . er/contract-region)) ;; shrink region
  :init
  (setq expand-region-fast-keys-enabled t))

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :init
   (let ((paths (split-string (or (getenv "TREE_SITTER_GRAMMAR_PATH") "") ":" t)))
    (setq treesit-extra-load-path
          (append paths treesit-extra-load-path)))
)

(use-package guix
  :ensure t		       ; Install 'guix' package if not already
  :config
  ;; Assuming Guix is installed and its environment variables are set up
  ;; (e.g., through your shell's .profile or Guix Home configuration)
  ;; This ensures Emacs-Guix can find Guile modules and Guix commands.

  ;; Optional: If you want to use Emacs-Guix for developing Guix itself
  ;; and have a Guix source checkout, similar to the 'geiser-guile' example.
  ;; Replace "~/src/guix" with the actual path to your Guix source.
  (setq guix-load-path "~/src/guix")	; For Guile modules

  ;; Auto-prettify store file names (e.g., /gnu/store/hash-package-version -> /gnu/store/...-package-version)
  ;;(guix-prettify-store-paths-mode 1)

  ;; Keybindings (optional, often M-x guix is enough to get to the popup)
  (global-set-key (kbd "C-c p") 'guix)	; Example global binding

  ;; You might want to enable `guix-devel-mode` for .scm files
  ;; to get better Guix-specific features when editing package definitions.
  (add-hook 'scheme-mode-hook (lambda ()
                                (when (string-match-p "\\.scm\\'" (buffer-file-name))
                                  (guix-devel-mode 1))))

  ;; If you're using Guix Home and want to edit your home configuration,
  ;; you might add its path here as well for Geiser/Guix development mode.
  ;; (add-to-list 'geiser-guile-load-path "~/.config/guix/current/share")
  ;; (add-to-list 'geiser-guile-load-path "~/my-guix-home-config-repo")
  )

(use-package geiser
  :ensure t
  :custom
  (geiser-default-implementation 'guile)
  (geiser-active-implementations '(guile))
  (geiser-implementations-alist '(((regexp "\\.scm$") guile)))
  :hook
  (scheme-mode . geiser-mode))

(use-package geiser-guile
  :ensure t ; Install geiser-guile if it's not already
  :config
  ;; Assuming the Guix checkout is in ~/src/guix.
  (add-to-list 'geiser-guile-load-path "~/src/guix"))

(use-package indent-bars
  :ensure t
  :hook ((yaml-mode . indent-bars-mode)
	 (python-mode . indent-bars-mode)))

;;; Dired
(use-package dired-narrow
  :ensure t
  :config
  (bind-keys :map dired-mode-map
	     ("f" . dired-narrow-fuzzy)))

;;; Undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (let ((undo-dir (expand-file-name "undo-tree/" (getenv "XDG_CACHE_HOME"))))
    (unless (file-directory-p undo-dir)
      (make-directory undo-dir t))
    (setq undo-tree-history-directory-alist `((".*" . ,undo-dir))
	  undo-tree-auto-save-history t))
  (global-undo-tree-mode))

(use-package paredit
  :ensure t
  ;; enable in all the major Lisp modes you care about
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode
          clojure-mode) . paredit-mode)
  :bind
  ;; let’s make “M-(” wrap the following sexp in parens
  (:map paredit-mode-map
        ("M-(" . paredit-wrap-round)
        ;; some handy defaults you can tweak:
        ("C-M-f" . paredit-forward) 
        ("C-M-b" . paredit-backward)
        ("C-)"   . paredit-forward-slurp-sexp)
        ("C-("   . paredit-forward-barf-sexp))
  :config
  ;; optional: show mismatched parens in fringe
  (show-paren-mode +1))

;;; Ivy and Consel
(use-package ivy
  :ensure t
  :bind (("C-s" . swiper)
	 ("C-S-s" . isearch-forward))
  :diminish ivy-mode
  :init (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind (("C-c g" . counsel-rg)))

;;; SmartParens
(use-package smartparens-mode
  :ensure smartparens  ;; install the package
  :hook (prog-mode
	 text-mode
	 markdown-mode
	 rustic-mode
	 terraform-mode) ;; add `smartparens-mode` to these hooks
  :bind ("C-<left>" . sp-forward-slurp-sexp)
  :config
  ;; load default config
  (require 'smartparens-config))

;;; Magit
(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status)))

;;; Rust
(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save t)
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

;;; Terraform
(use-package terraform-mode
  :ensure t
   :hook (terraform-mode . (lambda ()
                            (add-hook 'before-save-hook #'terraform-format-buffer nil t))))

;;; web-mode
(use-package web-mode
  :ensure t
  :mode (".svelte$"))

;;; yaml
(use-package yaml-ts-mode
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package json-ts-mode
  :mode ("\\.json\\'"))

(use-package dockerfile-ts-mode
  :mode ("\\Dockerfile\\'"))

;;; Prettier
(use-package prettier-js
  :ensure t
  :mode (("\\.tsx\\'" . prettier-js-mode)
	 ("\\.json\\'" . prettier-js-mode)))

(use-package tsx-ts-mode
  :mode "\\.tsx\\'")

;;; Lsp-mode
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((terraform-mode . lsp)
	 (tsx-ts-mode . lsp))
  :magic (".svelte$" . lsp)
  :commands lsp)

(use-package lsp-ui
  :ensure t)

(use-package devcontainer
  :ensure t
  :straight (devcontainer :type git :host github :repo "johannes-mueller/devcontainer.el"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight bold :height 98 :width normal)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'init.el)
;;; init.el ends here
