(in-package :text-edit)

(defun clipboard-set (lines)
  (sdl2-ffi.functions:sdl-set-clipboard-text (format nil "~{~a~%~}" lines)))

(defun clipboard-get ()
  (split-sequence:split-sequence #\NewLine
				 (sdl2-ffi.functions:sdl-get-clipboard-text)))

(defun clipboard-has-text? ()
  (sdl2-ffi.functions:sdl-has-clipboard-text))
