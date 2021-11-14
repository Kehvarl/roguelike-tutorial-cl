(in-package :roguelike-tutorial-cl)

(defclass component ()
  ((owner :initarg :owner :accessor component/owner)))

(defclass fighter (component)
  ((max-hp :initarg :max-hp :accessor fighter/max-hp :initform nil)
   (hp :initarg :hp :accessor fighter/hp)
   (defense :initarg :defense :accessor fighter/defense)
   (power :initarg :power :accessor fighter/power)))

(defclass basic-monster (component) ())

(defgeneric take-turn (component target map entities))
(defmethod take-turn ((component basic-monster) target map entities)
  (let* ((monster (component/owner component))
         (in-sight (tile/visible (aref (game-map/tiles map) (entity/x monster) (entity/y monster)))))
    (when in-sight
      (cond ((>= (distance-to monster target) 2)
             (move-towards monster (entity/x target) (entity/y target) map entities))
            ((> (fighter/hp (entity/fighter target)) 0)
             (format t "The ~A insults you! Your ego is damaged!~%" (entity/name (component/owner component))))))))

(defgeneric move-towards (e target-x target-y map entities))
(defmethod move-towards ((e entity) target-x target-y map entities)
  (with-slots (x y) e
    (let* ((dx (- target-x x))
           (dy (- target-y y))
           (distance (sqrt (+ (expt dx 2) (expt dy 2)))))
      (setf dx (round (/ dx distance))
            dy (round (/ dy distance)))
      (unless (or (blocked-p map (+ x dx) (+ y dy)))
        (move e dx dy)))))
