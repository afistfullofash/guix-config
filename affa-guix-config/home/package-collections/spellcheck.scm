(define-module (affa-guix-config home package-collections spellcheck)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages hunspell)

  #:export (spellcheck-packages))

(define spellcheck-packages
  (list
   aspell
   hunspell
   hunspell-dict-en))

