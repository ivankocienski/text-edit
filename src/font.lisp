(in-package :text-edit)

(defparameter *font-handle* nil)
(defparameter *font-renderer* nil)
(defparameter *font-color* '(255 255 255))

(defparameter *font-textures* nil)

(defun font-init (renderer)
  (sdl2-ttf:init)
  (setf *font-renderer* renderer
	*font-handle*
	(sdl2-ttf:open-font (asdf:system-relative-pathname 'text-edit
							   "data/vga.ttf")
			    16)
	*font-textures* (make-array 127))

  (dotimes (ch 127)
    (setf (aref *font-textures* ch)

	  (let ((surface))
	    (unwind-protect
		 (when (> ch 31)
		   (setf surface (sdl2-ttf:render-text-solid *font-handle*
							      (string (code-char ch))
							      255
							      255
							      255
							      0))

		   (when (> (sdl2:surface-width surface) 0)
		     
		     (sdl2:create-texture-from-surface *font-renderer*
						       surface)))
	    
		 (when surface (sdl2:free-surface surface)))))))

	    
	 
	

(defun font-color (r g b)
  (setf *font-color* (list r g b))
  (dotimes (ch 127)
    (let ((tex (aref *font-textures* ch)))
      (when tex
    
	(sdl2:set-texture-color-mod tex
				    r
				    g
				    b)))))

(defun font-draw-string (x y str &optional inverse)
  "draw string in loaded font"
  
  (let ((destination-rect (sdl2:make-rect x
				    y
				    9
				    16)))

    (when inverse
      (sdl2:set-render-draw-color *font-renderer*
				  100
				  100
				  100
				  255))
    (loop for ch across str
       do (progn
	    (let ((tex (aref *font-textures* (char-code ch))))
	      (when inverse
		(sdl2:render-fill-rect *font-renderer*
				       destination-rect))
	      
	      (when tex
		
		(sdl2:render-copy *font-renderer*
				  tex
				  :dest-rect destination-rect))
	      (incf (sdl2:rect-x destination-rect)
		    (sdl2:texture-width tex)))))))

