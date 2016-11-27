(in-package #:text-edit)

(defconstant +XRES+ 800)
(defconstant +YRES+ 600)

(defparameter *mod-shift* nil)
(defparameter *mod-control* nil)

(defun init (renderer)
  
  (setf *mod-shift*   nil
	*mod-control* nil)
  
  (timer-reset-all)
  (font-init renderer)
  (cursor-init renderer)

  (screen-init-all)
  
  (app-repaint))

(defun paint (renderer)
  (app-when-repaint ()
    
    (sdl2:set-render-draw-color renderer 0 0 0 255)
    (sdl2:render-clear renderer)

    (screen-invoke-paint renderer)
    
    (sdl2:render-present renderer)))

(defun key-down (sym)
  (when (or (sdl2:scancode= sym :scancode-lshift)
	    (sdl2:scancode= sym :scancode-lshift))
    (setf *mod-shift* t))

  (when (or (sdl2:scancode= sym :scancode-lctrl)
	    (sdl2:scancode= sym :scancode-rctrl))
    (setf *mod-control* t))

  (screen-invoke-key-down sym
			  *mod-shift*
			  *mod-control*))

(defun key-up (sym)
  (when (or (sdl2:scancode= sym :scancode-lshift)
	    (sdl2:scancode= sym :scancode-lshift))
    (setf *mod-shift* t))

  (when (or (sdl2:scancode= sym :scancode-lctrl)
	    (sdl2:scancode= sym :scancode-rctrl))
    (setf *mod-control* t))

  (screen-invoke-key-up sym))
    
(defun on-text (code-point)
  (unless *mod-control*
    (screen-invoke-text (string (code-char code-point))
			*mod-shift*
			*mod-control*)))

(defun main ()
  "entry point. run this."

  (log-init)
  (log-wr :info "text edit started")
  
  (sdl2:with-init (:video :events)
    
    (sdl2:with-window (window :w +XRES+
			      :h +YRES+
			      :title "SDL window"
			      :flags '(:shown))

      (sdl2:with-renderer (renderer window)

	(init renderer)
	(log-wr :info "entering main loop")

	(screen-show :editor)
	
	(let ((last-time (sdl2:get-ticks)))
	  (sdl2:with-event-loop (:method :poll)
	    (:textinput (:text code-point) (on-text code-point))
	    
	    (:keydown (:keysym sym) (key-down sym))
	    (:keyup   (:keysym sym) (key-up   sym))
	    
	    (:quit () t)
	    
	    (:idle ()
		   (let ((time-now (sdl2:get-ticks)))
		     (timer-tick-all (* (- time-now last-time) 0.001))
		     (setf last-time time-now))

		   (paint renderer))
	    
	    ))))))

;; (sdl2:quit)
