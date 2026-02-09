;;
;; This is all happily going the way of the dodo
;; Im am getting really into manifests
;; Just need:
;; - a proper way to load them into emacs automatically by project
;; - a way for starship to indicate im in a guix shell 
;; 
(define-module (affa-guix-config home package-collections programming)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages version-control)
  
  #:export (development-packages
	    git-packages
	    language-server-packages
	    rust-packages
	    common-lisp-packages))

(define common-lisp-packages
  (list sbcl
	sbcl-slynk
	sbcl-legit))

(define rust-packages
  ;; Unfortunantly it seems I need to pollute these
  ;; I currently dont know how to make emacs open a guix shell environment
  ;; when it is inside a relevant project
  (list rust
	`(,rust "cargo")
	`(,rust "tools")))

(define git-packages
  (list git
	`(,git "credential-libsecret")
	git-lfs
	pre-commit))

(define development-packages
  (append common-lisp-packages
	  rust-packages
	  git-packages))

(define language-server-packages
  (list rust-analyzer
	python-lsp-server
	guile-lsp-server
	sqls))



