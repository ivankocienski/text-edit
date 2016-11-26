(in-package :text-edit)

(defparameter *doc-lines* nil
  "Actual document lines as loaded from file")


(defun doc-init ()
  )

(defun doc-length ()
  (length *doc-lines*))

(defun doc-load (path)
  (log-wr :info "doc-load: path=~a" path)
  (with-open-file (read-file path)
    (labels ((read-line-to-doc ()
	       (let ((line (read-line read-file nil)))
		 (when line
		   (cons line (read-line-to-doc))))))

      (setf *doc-lines* (read-line-to-doc))))
  (buffer-setup (first *doc-lines*))
  (log-wr :info "lines=~d" (length *doc-lines*)))


(defun doc-update-line (pos line)
  (setf (nth pos *doc-lines*) line))

(defmacro doc-line (line-no &optional (doc-lines *doc-lines*))
  `(nth ,line-no ,doc-lines))

(defun doc-setup-cursor-line (cursor-line-number)
  (buffer-setup (doc-line cursor-line-number)))

(defun doc-valid-line? (cursor-line)
  (and (>= cursor-line 0)
       (< cursor-line (doc-length))))

(defun doc-backspace (cursor-line cursor-pos)
  (buffer-backspace cursor-pos)
  (doc-update-line cursor-line *buffer-line*))

;;(defun doc-delete ()
;;  )

(defun doc-insert-text (line pos text)
  (buffer-append pos text)
  (doc-update-line line *buffer-line*))

(defun doc-insert-lines (pos lines)
  (setf *doc-lines* (insert-at *doc-lines* pos lines)))

(defun doc-cut-lines (start end)
  (let ((head  (subseq *doc-lines* 0 start))
	(chunk (subseq *doc-lines* start (1+ end)))
	(tail  (if (< end (doc-length)) (subseq *doc-lines* (1+ end) (doc-length)))))

    (setf *doc-lines* (append head tail))
    chunk))

(defun doc-split-to-end (line pos)
  (let ((remains (buffer-cut-line pos (buffer-length))))
    (doc-update-line line *buffer-line*)
    remains))

(defun doc-split-to-start (line pos)
  (let ((remains (buffer-cut-line 0 pos)))
    (doc-update-line line *buffer-line*)
    remains))

(defun doc-delete ()
  (multiple-value-bind (start end) (select-normalized)
    (let ((start-char (cursor-char-number start))
	  (start-line (cursor-line-number start))
	  (end-char   (cursor-char-number end))
	  (end-line   (cursor-line-number end)))
      
      (if (select-single-line?)
	  (progn
	    (buffer-cut-line start-char end-char)
	    (doc-update-line start-line *buffer-line*))

	  ;; else multi-line
	  (progn
	    (doc-setup-cursor-line start-line)
	    (doc-split-to-end start-line start-char)

	    (doc-setup-cursor-line end-line)
	    (doc-split-to-start end-line end-char)
				
	    (let ((old-end (nth end-line *doc-lines*)))
				  
	      (doc-cut-lines (1+ start-line) end-line)

	      (let ((old-start (nth start-line *doc-lines*)))
		(doc-update-line start-line
				 (format nil "~a~a" old-start old-end)))))))))

  
(defun doc-copy ()
  (multiple-value-bind (start end) (select-normalized)
    (let ((start-char (cursor-char-number start))
	  (start-line (cursor-line-number start))
	  (end-char   (cursor-char-number end))
	  (end-line   (cursor-line-number end)))
      
      (if (select-single-line?)	  
	  (list (buffer-copy-line start-char end-char))
	  
	  ;; else multi-line
	  (progn
	    (let ((start-text (buffer-copy-line start-char
						nil
						(nth start-line *doc-lines*)))
		  (end-text (buffer-copy-line nil
					      end-char
					      (nth end-line *doc-lines*)))
		  (mid-text (subseq *doc-lines*
				    (1+ start-line)
				    end-line)))
	     
	      (append (list start-text)
		      mid-text
		      (list end-text))))))))

(defun doc-paste (line-pos char-pos lines)
  (if (= (length lines) 1)
      
      ;; single line
      (buffer-append char-pos (first lines) (doc-line line-pos))

      ;; multi line insert
      (let ((buffer-post-cursor (let ((cursor-line (doc-line line-pos)))
				  (buffer-cut-line char-pos
						   (buffer-length cursor-line)
						   cursor-line)))
	    (first-line (first lines))
	    (rest-lines (rest lines)))


	
	(setf (doc-line line-pos)
	      (string-cat (doc-line line-pos)
			  first-line))

	(setf (car (last rest-lines))
	      (string-cat (car (last rest-lines))
			  buffer-post-cursor))

	(doc-insert-lines (1+ line-pos) rest-lines))))
