(define-module (affa-guix-config home utils)
  #:use-module (gnu home)
  #:use-module (guix gexp)
  #:use-module (guix utils)

  #:export (home-directory
	    home-file-path
	    
	    config-file
	    module-file
	    
	    environment-variable-seperated-path))

(define home-directory (getenv "HOME"))

(define (home-file-path file)
  (string-append home-directory file))

(define %current-source-directory
  (current-source-directory))

(define %module-root-directory
  (string-append %current-source-directory "/.."))

(define (module-root-path path)
  (string-append %module-root-directory path))

(define* (module-file file #:key (recursive? #f) (select? (const #t)) name)
  (local-file (module-root-path file) name
              #:recursive? recursive?
              #:select? select?))

(define config-file-base-path (module-root-path "/files"))

(define (config-file-path file)
  (string-append config-file-base-path file))

(define* (config-file file #:key (recursive? #f) (select? (const #t)) name)
  (local-file (config-file-path file) name
              #:recursive? recursive?
              #:select? select?))

(define (environment-variable-seperated-path items)
  (string-join (map home-file-path items) ":"))
