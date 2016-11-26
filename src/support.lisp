(in-package :text-edit)

(defparameter true t)
(defparameter false nil)

(defun insert-at (lst index newelt)
  (let ((pre-pos  (subseq lst 0 index))
	(post-pos (subseq lst index (length lst))))

    (append pre-pos
	    (if (listp newelt)
		newelt
		(list newelt))
	    post-pos)))
    

(defun string-snip (in-string start-pos end-pos)

  (cond
    ;; ignore stupid inputs
    ((>= start-pos end-pos)
     (values in-string nil))
    
    ;; from first character to end-pos
    ((= start-pos -1)
     (let ((chomped (subseq in-string 0 end-pos))
	   (kept    (subseq in-string end-pos (length in-string))))

       (values kept chomped)))

    ;; from start-pos to end of line
    ((>= end-pos (length in-string))
     (let ((kept    (subseq in-string 0 start-pos))
	   (chomped (subseq in-string start-pos end-pos)))
       
       (values kept chomped)))
      
    ;; from start-pos to end-pos
    (t
     (let ((keep-head (subseq in-string 0 start-pos))
	   (keep-tail (subseq in-string end-pos (length in-string)))
	   (chomped   (subseq in-string start-pos end-pos)))
       
       (values (format nil "~a~a" keep-head keep-tail)
	       chomped)))))

(defun string-cat (&rest strings)
  (format nil "~{~a~}" strings))
