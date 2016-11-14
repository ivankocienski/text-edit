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
  (setf *doc-num-lines* (length *doc-lines*)
	*doc-view-starts-at* *doc-lines*)
  (buffer-setup (first *doc-lines*))
  (doc-update-cursor)
  (log-wr :info "lines=~d" (length *doc-lines*)))


(defun doc-update-line (pos line)
  (setf (nth pos *doc-lines*) line))

(defun doc-setup-cursor-line (cursor-line-number)
  (buffer-setup (nth cursor-line-number *doc-lines*)))

(defun doc-valid-line? (cursor-line)
  (and (>= cursor-line 0)
       (< cursor-line (doc-length))))

(defun doc-backspace ()
  (buffer-backspace)
  (select-clear)
  (doc-update-line *doc-cursor-offset* *buffer-line*)
  (doc-update-cursor)
  (app-repaint))

(defun doc-delete ()

  (if (select-active?)

      ;; do a 'cut'
      (if (select-single-line?)
	  (multiple-value-bind (cut-text remaining)
	      (buffer-cut-line (select-current-start-char)
			       (select-current-end-char))
	    
	    (declare (ignore cut-text))
	    (doc-update-line *doc-cursor-offset* remaining)
	    (app-repaint))

	  ;; multi-line cut
	  (let* ((start-line-pos (select-current-start-line))
		 (end-line-pos   (select-current-end-line))
		 (start-line (nth start-line-pos *doc-lines*))
		 (end-line   (nth end-line-pos   *doc-lines*)))

	    (format t "multi-line start-pos=~d  end-pos=~d~%" start-line-pos end-line-pos)
	    
	    (multiple-value-bind (start-cut start-remaining)
		(string-snip start-line
			     (select-current-start-char)
			     (length start-line))

	      (declare (ignore start-cut))
		       
	      (multiple-value-bind (end-cut end-remaining)
		  (string-snip end-line
			       -1
			       (select-current-end-char))

		(declare (ignore end-cut))

		(format t "start-remaining=~s~%" start-remaining)
		(format t "  end-remaining=~s~%" end-remaining)
		
		(let ((doc-head (subseq *doc-lines* 0 start-line-pos))
		      (doc-tail (subseq *doc-lines* end-line-pos (length *doc-lines*))))

		  (setf *doc-lines* (append doc-head
					    (list (format nil "~a~a" start-remaining end-remaining))
					    doc-tail)))
		(app-repaint)))))

      ;; do a delete char right
      (progn
	(doc-cursor-right)
	(doc-backspace))))

(defun doc-text (text)
  (buffer-append text)
  (select-clear)
  (doc-update-line *doc-cursor-offset* *buffer-line*)
  (doc-update-cursor)
  (app-repaint))

(defun doc-return ()
  (log-wr :info "doc-return")
  (let ((new-line (buffer-split-line-at-cursor)))
    (doc-update-line *doc-cursor-offset* *buffer-line*)
    (setf *doc-lines* (insert-at *doc-lines* (1+ *doc-cursor-offset*) new-line)
	  *doc-view-starts-at* (nthcdr *doc-view-offset* *doc-lines*))
    (incf *doc-cursor-offset*)
    (select-clear)
    (doc-update-cursor)
    (app-repaint)))

  
