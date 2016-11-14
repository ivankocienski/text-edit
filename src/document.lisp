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

(defun doc-split-to-end (line pos)
  (let ((remains (buffer-cut-line pos (buffer-length))))
    (doc-update-line line *buffer-line*)
    remains))

(defun doc-split-to-start (line pos)
  (let ((remains (buffer-cut-line 0 pos)))
    (doc-update-line line *buffer-line*)
    remains))
  
