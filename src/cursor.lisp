(in-package :text-edit)

(defparameter *cursor-width* 8)
(defparameter *cursor-height* 8)

;; TODO- make blinky

(defun cursor-draw-block (px py)
  (gl:disable :texture-2d)
  (gl:color 1 1 1)

  (let* ((px1 (* px  *cursor-width*))
	 (py1 (* py  *cursor-height*))
	 (px2 (+ px1 *cursor-width*))
	 (py2 (+ py1 *cursor-height*)))
	
    (gl:with-primitives :quads
      (gl:vertex px1 py1)
      (gl:vertex px2 py1)
      (gl:vertex px2 py2)
      (gl:vertex px1 py2)))

      
  (gl:enable :texture-2d))
