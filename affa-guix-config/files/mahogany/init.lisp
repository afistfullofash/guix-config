(in-package #:mahogany-user)

(uiop:launch-program "waybar")

(defun open-firefox (sequence seat)
  (declare (ignore sequence seat))
  (uiop:launch-program "firefox"))

(defun open-emacs (sequence seat)
  (declare (ignore sequence seat))
  (uiop:launch-program "emacs"))

(mahogany:add-to-kmap mahogany:*root-map*
	     (mahogany:kbd "e") #'open-emacs
	     (mahogany:kbd "f") #'open-firefox
	     (mahogany:kdb "c") #'mahogany:open-terminal)
