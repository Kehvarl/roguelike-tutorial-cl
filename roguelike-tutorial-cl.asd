;;;; roguelike-tutorial-cl.asd

(asdf:defsystem #:roguelike-tutorial-cl
  :description "A Common-Lisp implementation of the 2020 Python/TCOD Roguelike Tutorial"
  :author "Kehvarl <Kehvarl@Kehvarl.com>"
  :license  "CC-1"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-blt)
  :components ((:file "package")
               (:file "game-map")
               (:file "entity")
               (:file "fov")
               (:file "roguelike-tutorial-cl")))
