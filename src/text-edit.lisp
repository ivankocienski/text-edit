(in-package #:text-edit)

(defconstant +XRES+ 800)
(defconstant +YRES+ 600)

(defun init ()
  )

(defun paint ()
  )


(defun key-down (sym unicode mod)
  (case sym
    (:sdl-key-return    (buffer-newline))
    (:sdl-key-backspace (buffer-backspace))
    (:sdl-key-left      (buffer-cursor-left))
    (:sdl-key-right     (buffer-cursor-right))
    (:sdl-key-up        (doc-cursor-up))
    (:sdl-key-down      (doc-cursor-down))
    (t (buffer-append unicode))))

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

	(doc-init)
	(doc-load (asdf:system-relative-pathname 'text-edit "data/palwarp.c"))
	
	(timer-reset-all)
	(font-init renderer)
	(cursor-init renderer)
	
	(app-repaint)
	(log-wr :info "entering main loop")

	(let ((last-time (sdl2:get-ticks)))
	  (sdl2:with-event-loop (:method :poll)
	    (:textinput (:text text)
			(log-wr :debug "text='~a'" text)
			)
	    
	    ;;(:keydown (:keysym sym)
	    ;;	    (format t "keydown sym=~d~%" sym)
	    ;;	    )
	    
	    (:quit ()
		   t)
	    
	    (:idle ()
		   (let ((time-now (sdl2:get-ticks)))
		     (timer-tick-all (* (- time-now last-time) 0.001))
		     (setf last-time time-now))
		   
		   (app-when-repaint ()
		     ;;(paint)
		     (sdl2:set-render-draw-color renderer 0 0 0 255)
		     (sdl2:render-clear renderer)
		     
		     (font-color 255 255 255)
		     (sdl2:set-render-draw-color renderer 255 255 255 255)
		     (doc-draw)

		     (cursor-draw)
		     (sdl2:render-present renderer)))
	    
	    ))))))

;; (sdl2:quit)
