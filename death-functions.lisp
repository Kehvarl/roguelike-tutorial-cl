(in-package :roguelike-tutorial-cl)

(defun kill-player (player)
  (setf (entity/char player) #\%
        (entity/color player) (blt:red))
  (values "You died!" :player-dead))

(defun kill-monster (monster)
  (with-slots (char color blocks ai name) monster
    (let ((message (format nil "~A has died!~%" name)))
      (setf char #\%
            color (blt:red)
            blocks nil
            ai nil
            name (format nil "remains of ~A" name))
      message)))
