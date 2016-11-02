(in-package :text-edit)

(defparameter *timer-db* nil)

(defstruct timer
  period
  callback
  countdown)

(defun timer-reset-all ()
  (setf *timer-db* nil))

(defun timer-start (name period callback)
  (let ((new-timer (cons name
			 (make-timer :period period
				     :callback callback
				     :countdown 0))))
				     
    (setf *timer-db* (cons new-timer *timer-db*))))

(defmacro with-timer ((name period) &body body)
  `(timer-start ,name ,period (lambda () ,@body)))

(defun timer-stop (stop-name)
  (setf *timer-db* (remove-if (lambda (timer)
				(let ((name (car timer)))
				  (eq stop-name name)))
			      *timer-db*)))

(defun timer-tick-all (delta)
  (labels ((timer-update (db-timer)
	     (let ((name (car db-timer))
		   (timer (cdr db-timer)))
	       
	       (if (<= (timer-countdown timer) 0)
		   (progn
		     (funcall (timer-callback timer))
		     (setf (timer-countdown timer) (timer-period timer)))
		   
		   (decf (timer-countdown timer) delta))
	       (cons name timer))))
    
    (setf *timer-db* (mapcar #'timer-update *timer-db*))))
