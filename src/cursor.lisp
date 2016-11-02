(in-package :text-edit)

(defparameter *cursor-width* 9)
(defparameter *cursor-height* 16)
(defparameter *cursor-x* 0)
(defparameter *cursor-y* 0)
(defparameter *cursor-on* nil)
(defparameter *cursor-renderer* nil)

(defun cursor-set-pos (x y)
  (setf *cursor-x* x
	*cursor-y* y))

(defun cursor-init (renderer)
  (setf *cursor-renderer* renderer)
  (cursor-set-pos 0 0)
  (with-timer (:cursor-blink 0.5)
    (setf *cursor-on* (not *cursor-on*))
    (app-repaint)))

(defun cursor-draw ()
  (when *cursor-on*
    (let ((rect (sdl2:make-rect (* *cursor-x* *cursor-width*)
				(* *cursor-y* *cursor-height*)
				*cursor-width*
				*cursor-height*)))

      (sdl2:set-render-draw-color *cursor-renderer*
				  255
				  255
				  255
				  255)
      (sdl2:render-fill-rect *cursor-renderer* rect))))
