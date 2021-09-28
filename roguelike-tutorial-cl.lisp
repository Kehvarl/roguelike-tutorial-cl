;;;; roguelike-tutorial-cl.lisp

(in-package #:roguelike-tutorial-cl)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)

(defun draw()
  (blt:clear)
  (blt:refresh))

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "window.title = Common Lisp Roguelike Tutorial"))

(defun main()
  (blt:with-terminal
    (config)
    (loop :do
         (draw)
         (blt:key-case (blt:read)
                       (:escape (return))
                       (:close (return))))))
