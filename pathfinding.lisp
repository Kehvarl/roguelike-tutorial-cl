(in-package #:roguelike-tutorial-cl)

(defparameter *all-ditections*
  (list (cons 0 -1)
        (cons 0 1)
        (cons -1 0)
        (cons 1 0)
        (cons -1 -1)
        (cons -1 1)
        (cons 1 -1)
        (cons 1 1)))

(defclass node ()
  ((g :initform 0 :accessor node/g)
   (h :initform 0 :accessor node/h)
   (f :initform 0 :accessor node/f)
   (distance-from-parent :initarg :distance-from-parent :accessor node/distance-from-parent)
   (parent :initarg :parent :initform nil :accessor node/parent)
   (position :initarg :position :initform nil :accessor node/position)))
