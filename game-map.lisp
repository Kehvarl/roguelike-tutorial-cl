(in-package #:roguelike-tutorial-cl)

(defclass tile ()
  ((blocked :initarg :blocked
            :accessor tile/blocked
            :initform nil)
   (block-sight :initarg :block-sight
                :accessor tile/block-sight
                :initform nil)))

(defmethod initialize-instance :after ((tile tile) &rest initargs)
  (declare (ignore initargs))
  (with-slots (blocked block-sight) tile
    (if (null block-sight)
        (setf block-sight blocked))))

(defmacro map-tiles-loop ((map tile-val
                           &key (row-val (gensym)) (col-val (gensym))
                                (x-start 0) (y-start 0)
                                (x-end nil) (y-end nil))
                          &body body)
  `(loop :for ,col-val :from ,x-start :below (if (null ,x-end) (game-map/w ,map) ,x-end)
         :do
           (loop :for ,row-val :from ,y-start :below (if (null ,y-end) (game-map/h ,map) ,y-end)
                 :do
                    (let ((,tile-val (aref (game-map/tiles ,map) ,col-val ,row-val)))
                      (declare (ignorable ,tile-val))
                      ,@body))))

(defclass game-map ()
  ((width :initarg :w :accessor game-map/w)
   (height :initarg :h :accessor game-map/h)
   (tile :accessor game-map/tiles)))

(defmethod initialize-instance :after ((map game-map) &rest initargs)
  (declare (ignore initargs))
  (setf (game-map/tiles map) (make-array (list (game-map/w map)
                                               (game-map/h map)))))

(defmethod initialize-tiles ((map game-map))
  (map-tiles-loop (map tile :col-val x :row-val y)
    (setf (aref (game-map/tiles map) x y) (make-instance 'tile :blocked t))))

(defmethod blocked-p ((map game-map) x y)
  (tile/blocked (aref (game-map/tiles map) x y)))
