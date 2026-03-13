;; ** Themeing
;; This is a quick themeing system for stumpwm with a couple of theme options.

;; *** Basic StumpWM Themeing Sytem 
;; This gives us some very general functions to generate and apply /themes/ with.
(in-package :stumpwm-themeing)

(export '(*themes*
	  
	  add-theme
	  apply-theme
	  
	  set-system-themeing
	  toggle-system-themeing

	  set-key-seq-color

	  with-current-theme
	  get-color))
;; Visual
(defvar *themes* (make-hash-table))
(defvar *current-theme* 'default)

(defun add-theme (name theme)
  (setf (gethash name *themes*) theme))

      ;;; Colors based off spacemacs-dark-theme for emacs
(defclass theme ()
  ((fg
    :initarg :fg
    :initform "White"
    :accessor theme-fg
    :type string)
   (bg
    :initarg :bg
    :initform "Black"
    :accessor theme-bg
    :type string)

   (border
    :initarg :border
    :initform "White"
    :accessor theme-border
    :type string)

   (focus
    :initarg :focus
    :initform "White"
    :accessor theme-focus
    :type string)
   (unfocus
    :initarg :unfocus
    :initform "Black"
    :accessor theme-unfocus
    :type string)

   (mode-line-fg
    :initarg :mode-line-fg
    :initform stumpwm:*mode-line-foreground-color*
    :accessor theme-mode-line-fg
    :type string)
   (mode-line-bg
    :initarg :mode-line-bg
    :initform stumpwm:*mode-line-background-color*
    :accessor theme-mode-line-bg
    :type string)
   (mode-line-border
    :initarg :mode-line-border
    :initform stumpwm:*mode-line-border-color*
    :accessor theme-mode-line-border
    :type string)
   
   ;; These are the default colors of *colors* in order
   (black
    :initarg :black
    :initform "black"
    :accessor theme-black
    :type string)
   (red
    :initarg :red
    :initform "red"
    :accessor theme-red
    :type string)
   (green
    :initarg :green
    :initform "green"
    :accessor theme-green
    :type string)
   (yellow
    :initarg :yellow
    :initform "yellow"
    :accessor theme-yellow
    :type string)
   (blue
    :initarg :blue
    :initform "blue"
    :accessor theme-blue
    :type string)
   (magenta
    :initarg :magenta
    :initform "magenta"
    :accessor theme-magenta
    :type string)
   (cyan
    :initarg :cyan
    :initform "cyan"
    :accessor theme-cyan
    :type string)
   (white
    :initarg :white
    :initform "white"
    :accessor theme-white
    :type string)
   (custom-one
    :initarg :custom-one
    :initform "black"
    :accessor theme-custom-one
    :type string)
   (custom-two
    :initarg :custom-two
    :initform "white"
    :accessor theme-custom-two
    :type string)
   (low
    :initarg :low
    :accessor theme-low
    :type string)
   (medium
    :initarg :medium
    :accessor theme-medium
    :type string)
   (high
    :initarg :high
    :accessor theme-high
    :type string)
   (light-fg
    :initarg :light-fg
    :accessor theme-light-fg
    :type string)))

(defmethod initialize-instance :after ((obj theme) &key)
	   ;; Default prioritiyes to green, yellow, organge
	   (unless (slot-boundp obj 'low)
	     (setf (slot-value obj 'low) (slot-value obj 'green)))
	   (unless (slot-boundp obj 'medium)
	     (setf (slot-value obj 'medium) (slot-value obj 'yellow)))
	   (unless (slot-boundp obj 'high)
	     (setf (slot-value obj 'high) (slot-value obj 'red)))

	   (unless (slot-boundp obj 'light-fg)
	     (setf (slot-value obj 'light-fg) (slot-value obj 'fg))))

(defmethod get-color ((theme theme) color)
  (ecase color
    (:fg (theme-fg theme))
    (:bg (theme-bg theme))
    
    (:border (theme-border theme))
    (:focus  (theme-focus theme))
    (:unfocus (theme-unfocus theme))
    
    (:mode-line-fg (theme-mode-line-fg theme))
    (:mode-line-bg (theme-mode-line-bg theme))
    (:mode-line-border (theme-mode-line-border theme))
    
    (:black (theme-black theme))
    (:red (theme-red theme))
    (:green (theme-green theme))
    (:yellow (theme-yellow theme))
    (:blue (theme-blue theme))
    (:magenta (theme-magenta theme))
    (:cyan (theme-cyan theme))
    (:white (theme-white theme))
    
    (:custom-one (theme-custom-one theme))
    (:custom-two (theme-custom-two theme))
    
    (:low (theme-low theme))
    (:medium (theme-medium theme))
    (:high (theme-high theme))
    (:light-fg (theme-light-fg theme))))

(defmethod set-key-seq-color ((color string))
  (setf stumpwm:*key-seq-color* color) 
  (setf stumpwm:*which-key-format*
	(format nil "^(:fg \"~A\")" color))
  color)

(defmethod set-key-seq-color ((theme theme))
  (let ((key-seq-color (slot-value theme 'focus)))
    (set-key-seq-color key-seq-color)
    key-seq-color))

(defmethod apply-theme ((theme theme))
  (stumpwm:set-fg-color (slot-value theme 'fg))
  (stumpwm:set-bg-color (slot-value theme 'bg))
  (stumpwm:set-border-color (slot-value theme 'border))
  (stumpwm:set-focus-color (slot-value theme 'focus))
  (stumpwm:set-unfocus-color (slot-value theme 'unfocus))

  (setf stumpwm:*mode-line-foreground-color* (slot-value theme 'mode-line-fg)
      	stumpwm:*mode-line-background-color* (slot-value theme 'mode-line-bg)
      	stumpwm:*mode-line-border-color* (slot-value theme 'mode-line-border))
  
  (setf stumpwm:*colors* (list 
      		  (slot-value theme 'black)
      		  (slot-value theme 'red)
      		  (slot-value theme 'green)
      		  (slot-value theme 'yellow)
      		  (slot-value theme 'blue)
      		  (slot-value theme 'magenta)
      		  (slot-value theme 'cyan)
      		  (slot-value theme 'white)
      		  (slot-value theme 'custom-one)
      		  (slot-value theme 'custom-two)))
  (stumpwm:update-color-map (stumpwm:current-screen))

  (set-key-seq-color theme)
  
  t)

(defmethod apply-theme ((theme symbol))
  (apply-theme (get-theme 'default))
  (setf *current-theme* 'default)
  (apply-theme (get-theme theme))
  (setf *current-theme* theme))

(defun get-theme (theme-name)
  (gethash theme-name *themes*))

(defun with-current-theme ()
  (get-theme *current-theme*))

