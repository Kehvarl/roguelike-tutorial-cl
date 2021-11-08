(in-package :roguelike-tutorial-cl)

(defclass component ()
  ((owner :initarg :owner :accessor component/owner)))
