;;;; roguelike-tutorial-cl.lisp

(in-package #:roguelike-tutorial-cl)

(defparameter *game-name* "Common Lisp Roguelike Tutorial")
(defparameter *screen-width* 80)
(defparameter *screen-height* 50)

;; Terminal Window Settings.
(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "window.title = ~A" *game-name*))

;; Render (draw) our terminal window
(defun render-all (entities)
  (blt:clear)
  (mapc #'draw entities)
  (blt:refresh))

;; Simple keyboard-input handler.
(defun handle-keys ()
  (let ((action nil))
    (blt:key-case (blt:read)
                  (:up (setf action (list :move (cons 0 -1))))
                  (:down (setf action (list :move (cons 0 1))))
                  (:left (setf action (list :move (cons -1 0))))
                  (:right (setf action (list :move (cons 1 0))))
                  (:escape (setf action (list :quit t)))
                  (:close (setf action (list :quit t))))
    action))

;; Create a terminal window using the settings in our `config` function.
;; Then enter a loop where we draw the screen and check for a keypress
;; If the user closes the window or presses Escape, we exit.
;; Otherwise loop forever
(defun main()
  (blt:with-terminal
    (config)
    (loop
      :with player = (make-instance 'entity
                                    :x (/ *screen-width* 2)
                                    :y (/ *screen-height* 2)
                                    :char #\@
                                    :color (blt:white))
      :and  npc = (make-instance 'entity
                                 :x (- (/ *screen-width* 2) 5)
                                 :y (/ *screen-height* 2)
                                 :char #\@
                                 :color (blt:yellow))
      :with entities = (list player npc)
      :do
        (render-all entities)
        (let* ((action (handle-keys))
               (move (getf action :move))
               (exit (getf action :quit)))

          (if exit
            (return))

          (when move
            (move player (car move) (cdr move)))))))
