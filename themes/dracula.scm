(define-module (themes dracula)
  #:use-module (guix packages)
  #:use-module (guix git-download))

(define-public dracula-gtk-theme-repo
  (origin
    (uri (git-reference
	  (url "https://github.com/dracula/gtk.git")
	  (commit "918905b1d6175c506ad0f56fa9bb799a912e45f1")))
    (method git-fetch)
    (sha256
     (base32
      "0411xwm0zaaajlviil2za14113jlgksxayxvaxp73rrq8mi8x63i"))))

(define-public dracula-qt5-theme-repo
  (origin
    (uri (git-reference
	  (url "https://github.com/dracula/qt5.git")
	  (commit "7b25ee305365f6e62efb2c7aca3b4635622b778c")))
    (method git-fetch)
    (sha256
     (base32
      "00qlajbxj25w1bdhj8wc5r57g25gas6f1ax6wrzb4xcypw0j7xdm"))))

(define-public dracula-gtk-icons
  (origin
    (uri (git-reference
	  (url "https://github.com/m4thewz/dracula-icons.git")
	  (commit "6232e5217429a3ae6396c9e054f5338cecdbb7a5")))
    (method git-fetch)
    (sha256
     (base32
      "0n6mh6zypi5nhwl2wr20w77fcjfg682n8fm848awyqnn698hhrxv"))))

(define-public dracula-alacritty-theme-repo
  (origin
    (uri (git-reference
	  (url "https://github.com/dracula/alacritty.git")
	  (commit "c8a3a13404b78d520d04354e133b5075d9b785e1")))
    (method git-fetch)
    (sha256
     (base32
      "1pmk7m2bcwmnmhrbvnnm2znmyyh3vp42vvl1phvfbkz5yqz5jf2b"))))

(define-public dracula-starship-theme-repo
  (origin
    (uri (git-reference
	  (url "https://github.com/tpine/starship.git")
	  (commit "b9c82888b28d232a2ea2dd9ec03cca893834e761")))
    (method git-fetch)
    (sha256
     (base32
      "13i6alr7djb9h3vzav199i2kkxmzn004815z5cbc41lf7xvx2nc0"))))

(define-public dracula-lsd-theme-repo
  (origin
    (uri (git-reference
	  (url "https://github.com/dracula/lsd.git")
	  (commit "2b87711bdce8c89a882db720e4f47d95877f83a7")))
    (method git-fetch)
    (sha256
     (base32
      "10id0n5c9jyrah295dv2zahl97851kp24d513k3pyxbsy9nv0qml"))))

(define-public dracula-xresources-theme-repo
  (origin
    (uri (git-reference
	  (url "https://github.com/dracula/xresources.git")
	  (commit "539ef24e9b0c5498a82d59bfa2bad9b618d832a3")))
    (method git-fetch)
    (sha256
     (base32
      "1dkfa2q392vy7ky5kx0vd44xcb9c7x15z38x4acfma3f16q6vyg9"))))
