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

	(font-init renderer)
	(app-repaint)
	(log-wr :info "entering main loop")
	
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
		 (app-when-repaint ()
		   (paint)
		   (sdl2:render-clear renderer)
		   (sdl2:set-render-draw-color renderer 100 100 100 255)
		   (sdl2:render-fill-rect renderer (sdl2:make-rect 10
								   10
								   100
								   120))

		   (sdl2:set-render-draw-color renderer 255 255 255 255)

		   (font-color 255 0 0)
		   (font-draw-string 80 20 "Hello, World!")
		   
		   (font-color 0 255 0)
		   (font-draw-string 80 40 "Hello, World!")

		   (font-color 0 0 255)
		   (font-draw-string 80 60 "Hello, World!")

		   (sdl2:render-present renderer)))
	  
	  )))))

;; (sdl2:quit)
