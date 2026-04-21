(in-package :stumpwm-themeing)

(defun foreground-color (color)
  "If we have a list the theme has bright colors.
    They are a bit of a headache so ignore for now

    color is a string generally a hex"
  (if (typep color 'list)
      (format nil "^(:fg \"~A\")" (car color))
    (format nil "^(:fg \"~A\")" color)))

(defun background-color (color)
  "If we have a list the theme has bright colors.
    They are a bit of a headache so ignore for now

    color is a string generally a hex"
  (if (typep color 'list)
      (format nil "^(:bg \"~A\")" (car color))
    (format nil "^(:bg \"~A\")" color)))

