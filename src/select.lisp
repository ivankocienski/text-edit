(in-package :text-edit)

(defstruct select
  start
  end)

(defparameter *current-select* nil)

(defun select-begin (cursor)
  (log-wr :info "select-start")
  (setf *current-select* (make-select :start (copy-cursor cursor)
				      :end   (copy-cursor cursor))))	

(defun select-active? ()
  (not (null *current-select*)))

(defun select-current-start-line ()
  (cursor-line-number
   (select-start *current-select*)))

(defun select-current-end-line ()
  (cursor-line-number
   (select-end *current-select*)))

(defun select-current-start-char ()
  (cursor-char-number
   (select-start *current-select*)))

(defun select-current-end-char ()
  (cursor-char-number
   (select-end *current-select*)))

(defun select-single-line? ()
  (when (select-active?)
    (= (select-current-start-line)
       (select-current-end-line))))


(defun select-update (cursor)
  (when (select-active?)
    (setf (select-end *current-select*) (copy-cursor cursor))))

(defun select-finish ()
  (setf *current-select* nil))

(defun select-forward? ()
  (let ((start-line (select-current-start-line))
	(end-line   (select-current-end-line)))
    (if (= start-line end-line)
	(> (select-current-end-char) (select-current-start-char))
	(> end-line start-line))))

(defun select-normalized ()
  (if (select-forward?)
      (values (select-start *current-select*)
	      (select-end   *current-select*))
      (values (select-end   *current-select*)
	      (select-start *current-select*))))
      
(defun select-highlight-for-line (line-no)
  (when (select-active?)
    (multiple-value-bind (cur-start cur-end) (select-normalized)
      
      (let ((start-line (cursor-line-number cur-start))
	    (end-line   (cursor-line-number cur-end)))

	(cond
	  ((= start-line end-line)
	   (when (= line-no start-line)
	     (cons (1- (cursor-char-number cur-start))
		   (cursor-char-number cur-end))))

	  ((= start-line line-no)
	   (cons (1- (cursor-char-number cur-start))
		 1000))

	  ((= end-line line-no)
	   (cons -1
		 (cursor-char-number cur-end)))

	  ((< start-line line-no end-line)
	   (cons -1 1000))
	  
	  )))))

      

