(define-module (affa-guix-config home package-collections terminal)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wm)

  #:export (terminal-packages
	    zsh-plugin-packages))

(define terminal-packages
  (list
   ;; Top Replacement
   bottom
   ;; ls replacement
   lsd
   ;; cat replacement
   bat
   ;; ps replacement
   procs
   ;; fuzzy finding
   fzf
   fzf-tab
   ;; grep replacement
   ripgrep
   mpv
   ;; prompt replacement
   starship
   zip
   unzip

   stumpish))

(define zsh-plugin-packages
  (list zsh-autosuggestions
	zsh-completions
	zsh-history-substring-search
	zsh-syntax-highlighting))
