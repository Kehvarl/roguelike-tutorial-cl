(in-package #:roguelike-tutorial-cl)

(defclass entity ()
  ((name :initarg :name :accessor entity/name)
   (x :initarg :x :accessor entity/x)
   (y :initarg :y :accessor entity/y)
   (char :initarg :char :accessor entity/char)
   (color :initarg :color :accessor entity/color)
   (blocks :initarg :blocks :accessor entity/blocks :initform nil)))

;; Adjust the X,Y position of an entity by the indicated amount.
(defmethod move ((e entity) dx dy)
  (incf (entity/x e) dx)
  (incf (entity/y e) dy))

;; Draw a given entity on the screen
(defgeneric draw (e map))
(defmethod draw ((e entity) (map game-map))
  (with-slots (x y char color) e
    (if (tile/visible (aref (game-map/tiles map) x y))
      (setf (blt:background-color) (blt:cell-background-color x y)
            (blt:color) color
            (blt:cell-char x y) char))))
