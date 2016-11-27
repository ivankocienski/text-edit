(in-package :text-edit)

(define-screen-init (:editor)
  (doc-init)
  (doc-load (asdf:system-relative-pathname 'text-edit "data/palwarp.c")))

(define-screen-activate (:editor previous-screen data)
  )

(define-screen-paint (:editor renderer)
  (font-color 255 255 255)
  (view-draw *doc-lines*)

  (view-draw-status-line (cursor-current-char-number)
			 (cursor-current-line-number)
			 "(untitled)")
  (cursor-draw))

(define-screen-key-down (:editor key mod-shift mod-control)
  (on-key-do (key)
    (:scancode-left      (cursor-move-left  mod-shift))
    (:scancode-right     (cursor-move-right mod-shift))
    (:scancode-down      (cursor-move-vertical +CURSOR-GO-DOWN+ mod-shift))
    (:scancode-up        (cursor-move-vertical +CURSOR-GO-UP+   mod-shift))
    (:scancode-backspace (cursor-backspace))
    (:scancode-return    (cursor-newline))
    (:scancode-lshift    (cursor-select-begin))
    (:scancode-rshift    (cursor-select-begin))
    (:scancode-escape    (cursor-select-finish))
    (:scancode-delete    (cursor-delete))
    (:scancode-home      (cursor-move-home mod-shift))
    (:scancode-end       (cursor-move-end  mod-shift))
    
    (:scancode-c (when mod-control (cursor-copy)))
    (:scancode-v (when mod-control (cursor-paste)))))


(define-screen-key-up (:editor key)
    )

(define-screen-text (:editor str mod-shift mod-control)
  (when (not mod-control)
    (cursor-insert-text str)))

