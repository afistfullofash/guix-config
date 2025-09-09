;;; init.el --- Summary
;;; Commentary:
;;; Entry Point for Emacs customization

;;; Code:

;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display
(use-package emacs             ; the built-in pseudo-package
  ;; --------------------------------------------------------- ;;
  ;; Code that must run early (before other packages load)
  ;; --------------------------------------------------------- ;;
  :init
  ;; Minor modes / commands ----------------------------------
  (context-menu-mode 1)
  (global-hl-line-mode 1)
  (global-display-line-numbers-mode 1)
  (menu-bar-mode   -1)
  (tool-bar-mode   -1)
  (scroll-bar-mode -1)
  (tooltip-mode    -1)

  ;; Backup & auto-save directories --------------------------
  (let* ((backup-dir      (expand-file-name "~/.tmp/emacs/backups/"))
         (auto-saves-dir  (expand-file-name "~/.tmp/emacs/auto-saves/")))
    (dolist (dir (list backup-dir auto-saves-dir))
      (unless (file-directory-p dir)      ; create if missing
        (make-directory dir t)))
    (setq backup-directory-alist        `(("." . ,backup-dir))
          auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
          auto-save-list-file-prefix     (concat auto-saves-dir ".saves-")
          tramp-backup-directory-alist   `((".*" . ,backup-dir))
          tramp-auto-save-directory      auto-saves-dir))

  ;; Enable region case-changing commands without prompt -----
  (put 'upcase-region   'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;; --------------------------------------------------------- ;;
  ;; Plain variable customisations
  ;; --------------------------------------------------------- ;;
  :custom
  (enable-recursive-minibuffers   t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (inhibit-startup-message t)
  (initial-scratch-message  "")
  (enable-local-variables :all)
  (enable-local-eval       t)

  ;; --------------------------------------------------------- ;;
  ;; Faces
  ;; --------------------------------------------------------- ;;
  :custom-face
  (default
   ((t (:family "DejaVuSansM Nerd Font Mono"
                :weight bold
                :height 98)))))

(use-package flycheck
  :hook (after-init . global-flycheck-mode))

;;; Dired
(use-package dired-narrow
  :config
  (bind-keys :map dired-mode-map
	     ("f" . dired-narrow-fuzzy)))

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
	 ("\\.json\\'" . json-ts-mode))
  :init
  (let ((paths (split-string (or (getenv "TREE_SITTER_GRAMMAR_PATH") "") ":" t)))
    (setq treesit-extra-load-path
          (append paths treesit-extra-load-path))))


;;; yaml
(use-package yaml-ts-mode
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package json-ts-mode
  :mode ("\\.json$"))

(use-package dockerfile-ts-mode
  :mode ("\\Dockerfile\\'"))

(use-package tsx-ts-mode
  :mode "\\.tsx\\'")

(use-package vertico
  :init (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :bind (("C-s" . consult-line)
	 ("C-y" . consult-yank-from-kill-ring)
	 ("C-x b" . consult-buffer)
	 ("C-c g" . consult-ripgep)))

;; CORFU: Popup UI for in-buffer completion
(use-package corfu
  :init
  (global-corfu-mode) ;; enables Corfu in all buffers
  ;; :custom
  ;; (corfu-auto t)                ;; enable auto popup
  ;; (corfu-cycle t)               ;; allow cycling through candidates
  ;; (corfu-preselect-first t)
  ;; (corfu-quit-at-boundary nil)
  ;; (corfu-quit-no-match 'separator)
  ;; (corfu-scroll-margin 5)
  ;; (corfu-max-width 80)
  ;; :bind
  ;; (:map corfu-map
  ;;       ("TAB" . corfu-next)
  ;;       ([tab] . corfu-next)
  ;;       ("S-TAB" . corfu-previous)
  ;;       ([backtab] . corfu-previous))
  )

;; CAPE: Add extra completion sources to completion-at-point-functions
(use-package cape
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :init
  ;; Add useful defaults to `completion-at-point-functions`
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; Optionally:
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  )

;;; Magit
(use-package magit
  :bind (("C-c m" . magit-status)))

(use-package paredit
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

(use-package geiser
  :custom
  (geiser-default-implementation 'guile)
  (geiser-active-implementations '(guile))
  (geiser-implementations-alist '(((regexp "\\.scm$") guile)))
  :hook
  (scheme-mode . geiser-mode))

(use-package geiser-guile
  :config
  ;; Assuming the Guix checkout is in ~/src/guix.
  (add-to-list 'geiser-guile-load-path "~/src/guix"))

(use-package guix
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

(use-package terraform-mode
  :hook (terraform-mode . (lambda ()
                            (add-hook 'before-save-hook #'terraform-format-buffer nil t))))

(use-package rustic
  :config
  (setq rustic-format-on-save t)
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

;;; web-mode
(use-package web-mode
  :mode (".svelte$"))

;;; Prettier
(use-package prettier-js
  :hook ((js-mode . prettier-js-mode)
	 (ts-mode . prettier-js-mode)
	 (json-ts-mode . prettier-js-mode)))

;;; Lsp-mode
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((terraform-mode . lsp)
	 (tsx-ts-mode . lsp))
  :magic (".svelte$" . lsp)
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-scheme
  :after lsp-mode
  :custom
  ;; One of: "guile"  "chicken"  "gambit"  "chez"  "racket" …
  ;; Pick the implementation you'll use most often.
  (lsp-scheme-implementation "guile")  ; change to "chicken" etc. if needed
  ;; If you keep multiple Schemes, make it project-specific:
  ;; (dir-locals-set-class-variables
  ;;  'my-scheme
  ;;  '((scheme-mode . ((lsp-scheme-implementation . "chicken")))))
  ;; (dir-locals-set-directory-class "/path/to/project/" 'my-scheme)
  )

(use-package org
  :defer t
  :config
  ;; Enable shell, Scheme and Emacs-Lisp in Org Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell       . t)
     (scheme      . t)
     (emacs-lisp  . t)))

  ;; Suppress confirmation prompts
  (setq org-confirm-babel-evaluate nil

        ;; Shell defaults
        org-babel-sh-command "bash"
        org-babel-default-header-args:sh
        '((:results . "output replace")
          (:exports . "both")
          (:session . nil)
          (:cache . "no"))

        ;; Scheme defaults (override `org-babel-scheme-command` if you use another impl)
        org-babel-scheme-command "guile"
        org-babel-default-header-args:scheme
        '((:results . "output replace")
          (:exports . "both")
          (:session . nil)
          (:cache . "no"))

        ;; Emacs-Lisp defaults
        ;; (no external REPL, just evaluates in the current Emacs session)
        org-babel-default-header-args:emacs-lisp
        '((:results . "output replace")
          (:exports . "both")
          (:cache   . "no"))))

(use-package expand-region
  :bind (("C-=" . er/expand-region)  ;; grow region (like many IDEs)
         ("C--" . er/contract-region)) ;; shrink region
  :init
  (setq expand-region-fast-keys-enabled t))

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (let ((undo-dir (expand-file-name "undo-tree/" (getenv "XDG_CACHE_HOME"))))
    (unless (file-directory-p undo-dir)
      (make-directory undo-dir t))
    (setq undo-tree-history-directory-alist `((".*" . ,undo-dir))
	  undo-tree-auto-save-history t))
  (global-undo-tree-mode))

(use-package indent-bars
  :hook ((yaml-mode . indent-bars-mode)
  	 (python-mode . indent-bars-mode)))

;;; Themeing
(use-package doom-themes
  :init
  (load-theme 'doom-dracula t)
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
	doom-themes-enable-italic t
	doom-vibrant-brighter-modeline nil
	org-hide-leading-stars nil) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package nyan-mode
  :init
  ;; Fix up Nyan Cat cause she's pretty
  (setq nyan-animate-nyancat t
    	nyan-wavy-trail t)
  (nyan-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init.el)
;;; init.el ends here
