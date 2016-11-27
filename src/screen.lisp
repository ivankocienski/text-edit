(in-package :text-edit)

(defparameter *screen-db* (make-hash-table))
(defparameter *screen-current* nil)
(defparameter *screen-return* nil)

(defstruct screen
  ;;name
  data           ;; Screen specific data slot
  init-func      ;; Called once when app starts up
  paint-func     ;; Called when screen needs painting
  key-down-func  ;; Called when user presses key
  key-up-func    ;; Called when user releases key
  text-func      ;; Called when user enters text
  activate-func) ;; Called when screen is about to be shown

;;
;; invoking functions
;;

(defmacro invoke-screen-callback (name &rest args)
  `(let ((callback (,name *screen-current*)))
     (when callback
       (funcall callback ,@args))))

(defun screen-invoke-paint (renderer)
  (invoke-screen-callback screen-paint-func renderer))

(defun screen-invoke-key-down (key mod-shift mod-control)
  (invoke-screen-callback screen-key-down-func key mod-shift mod-control))

(defun screen-invoke-key-up (key)
  (invoke-screen-callback screen-key-up-func key))

(defun screen-invoke-text (key mod-shift mod-control)
  (invoke-screen-callback screen-text-func key mod-shift mod-control))



;;
;; defining functions
;;

(defmacro screen-set-property (name property function)
  (let ((screen-var (gensym "screen")))
    `(let ((,screen-var (gethash ,name
				 *screen-db*
				 (make-screen))))

       (setf (,property ,screen-var) ,function
	     (gethash ,name *screen-db*) ,screen-var))))


(defmacro define-screen-paint ((name render-var) &body body)
  `(screen-set-property ,name
			screen-paint-func
			(lambda (,render-var)
			  (declare (ignorable ,render-var))
			  ,@body)))

(defmacro define-screen-key-down ((name key-var mod-shift-var mod-control-var) &body body)
  `(screen-set-property ,name
			screen-key-down-func
			(lambda (,key-var ,mod-shift-var ,mod-control-var)
			  (declare (ignorable ,key-var ,mod-shift-var ,mod-control-var))
			  ,@body)))

(defmacro define-screen-key-up ((name key-var) &body body)
  `(screen-set-property ,name
			screen-key-up-func
			(lambda (,key-var)
			  (declare (ignorable ,key-var))
			  ,@body)))

(defmacro define-screen-text ((name str-var mod-shift-var mod-control-var) &body body)
  `(screen-set-property ,name
			screen-text-func
			(lambda (,str-var ,mod-shift-var ,mod-control-var)
			  (declare (ignorable ,str-var ,mod-shift-var ,mod-control-var))
			  ,@body)))

(defmacro define-screen-init ((name) &body body)
  `(screen-set-property ,name
			screen-init-func
			(lambda ()
			  ,@body)))
  
(defmacro define-screen-activate ((name previous-var data-var) &body body)
  `(screen-set-property ,name
			screen-activate-func
			(lambda (,previous-var ,data-var)
			  (declare (ignorable ,previous-var ,data-var))
			  ,@body)))
  

;;
;; misc
;;

(defmacro screen-current-data ()
  `(screen-data *screen-current*))

(defun screen-show (name &key data return-to)

  (labels ((invoke-activate (screen)
	     (let ((activate-func (screen-activate-func screen)))
	       (when activate-func (funcall activate-func nil data)))))

  (if (eq name :return)
      (if *screen-return*
	  (progn
	    (setf *screen-current* *screen-return*
		  *screen-return* nil)
	    (invoke-activate *screen-current*))

	  ;; else
	  (error "No screen to return to!"))

      ;; else
      (let ((new-screen (gethash name *screen-db*)))
	(if new-screen
	    (progn
	      (setf *screen-current* new-screen
		    *screen-return* return-to)
	      (invoke-activate *screen-current*))

	    ;; else
	    (error (format nil "screen ~s not defined" name)))))))



(defun screen-init-all ()
  (loop for key being the hash-keys of *screen-db*
     do (let* ((screen (gethash key *screen-db*))
	       (func (screen-init-func screen)))
	  
	  (when func (funcall func)))))


(defun screen-reset-db ()
  (setf *screen-db* (make-hash-table)))

;;(screen-reset-db)
