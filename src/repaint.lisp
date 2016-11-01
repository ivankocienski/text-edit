(in-package :text-edit)

(defparameter *app-repaint* nil)

(defun app-repaint ()
  (setf *app-repaint* t))

(defmacro app-when-repaint (() &body body)
  `(when *app-repaint*
     (setf *app-repaint* nil)
     ,@body))
