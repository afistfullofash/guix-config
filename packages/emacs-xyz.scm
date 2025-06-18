(define-module (packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages emacs-xyz))

(define-public emacs-nyan-mode-1.1.4
  (package
    (inherit emacs-nyan-mode)
    (name "emacs-nyan-mode-1.1.4")
    (version "1.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TeMPOraL/nyan-mode/")
             (commit "09904af23adb839c6a9c1175349a1fb67f5b4370")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03xp4dvq3y3q9xyb6pm9m5gb756rvbxcqk52ind08n7prqv4w1lp"))))))
