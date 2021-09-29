;;;; roguelike-tutorial-cl.lisp

(in-package #:roguelike-tutorial-cl)

(defparameter *game-name* "Common Lisp Roguelike Tutorial")
(defparameter *screen-width* 80)
(defparameter *screen-height* 50)

;; Draw our terminal window
(defun draw()
  (blt:clear)
  (setf (blt:color) (blt:white)
        (blt:cell-char 10 10) #\@)
  (blt:refresh))

;; Terminal Window Settings.
(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "window.title = ~A" *game-name*))

;; Create a terminal window using the settings in our `config` function.
;; Then enter a loop where we draw the screen and check for a keypress
;; If the user closes the window or presses Escape, we exit.
;; Otherwise loop forever
(defun main()
  (blt:with-terminal
    (config)
    (loop :do
         (draw)
         (blt:key-case (blt:read)
                       (:escape (return))
                       (:close (return))))))
