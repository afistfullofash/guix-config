(in-package :stumpwm-mode-line)


(defun ml-fmt-safe-change (change)
  "For a given mode line msg wrap it in push and pops so we can reset changes"
  (format nil "^(:push)~A^(:pop)" change))

