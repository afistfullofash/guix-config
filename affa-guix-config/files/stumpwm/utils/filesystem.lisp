(in-package :stumpwm-utils)

(defun mkdir-p (p)
  "Creates a directory and then returns its path"
  (ensure-directories-exist p)
  p)


(defun xdg-state-home (&rest more)
    "Returns an absolute pathname for the directory containing user-specific state files.
Adapted from uiop:xdg-data-home"
    (uiop:resolve-absolute-location
     `(,(or (uiop:getenv-absolute-directory "XDG_STATE_HOME")
            (uiop:os-cond
             ((uiop:os-windows-p) (uiop:get-folder-path :local-appdata))
             (t (uiop:subpathname (user-homedir-pathname) ".local/state/"))))
       ,more)))
