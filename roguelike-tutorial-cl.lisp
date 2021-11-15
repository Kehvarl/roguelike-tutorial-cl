;;;; roguelike-tutorial-cl.lisp

(in-package #:roguelike-tutorial-cl)

(defparameter *game-name* "Common Lisp Roguelike Tutorial")
(defparameter *screen-width* 80)
(defparameter *screen-height* 50)

(defparameter *map-width* 80)
(defparameter *map-height* 45)

(defparameter *room-max-size* 10)
(defparameter *room-min-size* 6)
(defparameter *max-rooms* 30)
(defparameter *max-enemies-per-room* 5)

(deftype game-states () '(member :player-turn :enemy-turn :exit))

;; Terminal Window Settings.
(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "window.title = ~A" *game-name*))

;; Tile Colors to help distinguish things
(defparameter *color-map* (list :dark-wall (blt:rgba 0 0 100)
                                :dark-ground (blt:rgba 50 50 150)
                                :light-wall (blt:rgba 130 110 50)
                                :light-ground (blt:rgba 200 180 50)))

;; Render (draw) our terminal window
(defun render-all (entities map)
  (blt:clear)
  (dotimes (y (game-map/h map))
    (dotimes (x (game-map/w map))
       (let* ((tile (aref (game-map/tiles map) x y))
              (wall (tile/blocked tile))
              (visible (tile/visible tile))
              (explored (tile/explored tile)))
         (cond
           (visible
            (if wall
              (setf (blt:background-color) (getf *color-map* :light-wall))
              (setf (blt:background-color) (getf *color-map* :light-ground)))
            (setf (blt:cell-char x y) #\Space))

           (explored
            (if wall
             (setf (blt:background-color) (getf *color-map* :dark-wall))
             (setf (blt:background-color) (getf *color-map* :dark-ground)))
            (setf (blt:cell-char x y) #\Space))))))

  (mapc #'(lambda (entity) (draw entity map)) entities)

  (setf (blt:background-color) (blt:black))
  (blt:refresh))

;; Simple keyboard-input handler.
(defun handle-keys ()
  (when (blt:has-input-p)
    (blt:key-case (blt:read)
                  ((or :up :w) (list :move (cons 0 -1)))
                  ((or :down :s) (list :move (cons 0 1)))
                  ((or :left :a) (list :move (cons -1 0)))
                  ((or :right :d) (list :move (cons 1 0)))
                  (:escape (list :quit t))
                  (:close (list :quit t)))))


;;The game-tick handles each turn:
;; Render the map
;; Check for any player inputs
;; Perform any needed actions
;; Return to our main loop.  Pass along the current value of `exit`
(defun game-tick (player entities map game-state)
  (declare (type game-states game-state))
  (render-all entities map)
  (let* ((action (handle-keys))
         (move (getf action :move))
         (exit (getf action :quit)))

    (when (and move (eql game-state :player-turn))
      (let ((destination-x (+ (entity/x player) (car move)))
            (destination-y (+ (entity/y player) (cdr move))))
        (unless (blocked-p map destination-x destination-y)
          (let ((target (blocking-entity-at entities destination-x destination-y)))
            (cond (target
                   (format t "~A kicks the ~A.~%" (entity/name player) (entity/name target)))
                  (t
                   (move player (car move) (cdr move))
                   (fov map (entity/x player) (entity/y player))))
            (setf game-state :enemy-turn)))))
    (when exit
      (setf game-state :exit))

    (when (eql game-state :enemy-turn)
      (dolist (entity (remove-if-not #'entity/ai entities))
        (take-turn (entity/ai entity) player map entities))
      (setf game-state :player-turn)))

  game-state)



;; Create a terminal window using the settings in our `config` function.
;; Then enter a loop where we let our game-tick perform its duties.
;; If an exit codition comes back from our tick, quit the program.
;; Otherwise loop forever
(defun main()
  (blt:with-terminal
    (config)
    (let* ((fighter-component (make-instance 'fighter
                                             :hp 30
                                             :defense 2
                                             :power 5))
           (player (make-instance 'entity
                           :name "Player"
                           :x (/ *screen-width* 2)
                           :y (/ *screen-height* 2)
                           :char #\@
                           :color (blt:white)
                           :blocks t
                           :fighter fighter-component))

           (entities (list player))
           (map (make-instance 'game-map :w *map-width* :h *map-height*)))
      (make-map map *max-rooms* *room-min-size* *room-max-size* *map-width* *map-height* player entities *max-enemies-per-room*)
      (fov map (entity/x player) (entity/y player))
      (do ((game-state :player-turn (game-tick player entities map game-state)))
          ((eql game-state :exit))))))
