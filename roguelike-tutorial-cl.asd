;;;; roguelike-tutorial-cl.asd

(asdf:defsystem #:roguelike-tutorial-cl
  :description "A Common-Lisp implementation of the 2020 Python/TCOD Roguelike Tutorial"
  :author "Kehvarl <Kehvarl@Kehvarl.com>"
  :license  "CC-1"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-blt #:queues.priority-queue)
  :components ((:file "package")
               (:file "game-map")
               (:file "pathfinding")
               (:file "entity")
               (:file "components")
               (:file "fov")
               (:file "death-functions")
               (:file "roguelike-tutorial-cl")))
