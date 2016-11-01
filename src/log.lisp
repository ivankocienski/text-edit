(in-package :text-edit)

(defparameter *log-output* *standard-output*)

(defun log-init (&key (output *standard-output*))
  (setf *log-output* output))

(defun log-wr (level format &rest args)
  (format *log-output* "~a: ~a~%"
	  level
	  (apply #'format nil format args)))
