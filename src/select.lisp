(in-package :text-edit)

(defstruct select
  start-line
  start-char
  end-line
  end-char)

(defparameter *current-select* nil)

(defun select-start ()
  (log-wr :info "select-start")
  (setf *current-select* (make-select :start-line *doc-cursor-offset*
				      :start-char *buffer-cursor-pos*
				      :end-line *doc-cursor-offset*
				      :end-char *buffer-cursor-pos*)))
	

(defun select-active? ()
  (not (null *current-select*)))

(defun select-current-start-line ()
  (select-start-line *current-select*))

(defun select-current-end-line ()
  (select-end-line *current-select*))

(defun select-current-start-char ()
  (select-start-char *current-select*))

(defun select-current-end-char ()
  (select-end-char *current-select*))

(defun select-single-line? ()
  (when (select-active?)
    (= (select-current-start-line)
       (select-current-end-line))))


(defun select-update ()
  (when (select-active?)
    (setf (select-end-line *current-select*) *doc-cursor-offset*
	  (select-end-char *current-select*) *buffer-cursor-pos*)))

(defun select-clear ()
  (setf *current-select* nil))

(defun select-highlight-for-line (line-no)
  (when (select-active?)
    (let ((start-line (select-start-line *current-select*))
	  (end-line   (select-end-line   *current-select*)))

      (cond
	((= start-line end-line)
	 (when (= line-no start-line)
	   (cons (select-start-char *current-select*)
		 (select-end-char   *current-select*))))

	((= start-line line-no)
	 (cons (select-start-char *current-select*)
	       1000))

	((= end-line line-no)
	 (cons -1
	       (select-end-char *current-select*)))

	((< start-line line-no end-line)
	 (cons -1 1000))
	
	))))

      

