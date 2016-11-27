(asdf:defsystem #:text-edit
  :description "because emacs"
  :author "Ivan K."
  :license ""
  :serial t
  :depends-on (sdl2 sdl2-ttf split-sequence)
  :components ((:file "src/package")
	       ;; support stuff
	       (:file "src/support")
	       (:file "src/log")
	       (:file "src/font")
	       (:file "src/screen")
	       (:file "src/repaint")
	       (:file "src/timer")

	       ;; text editor stuff
	       (:file "src/clipboard")
	       (:file "src/view")
	       (:file "src/buffer")
	       (:file "src/document")
	       (:file "src/cursor")
	       (:file "src/select")

	       ;; screens
	       (:file "src/text-edit")

	       ;; final main function
	       (:file "src/main")))

