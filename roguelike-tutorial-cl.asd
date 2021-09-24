;;;; roguelike-tutorial-cl.asd

(asdf:defsystem #:roguelike-tutorial-cl
  :description "Describe roguelike-tutorial-cl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-blt)
  :components ((:file "package")
               (:file "roguelike-tutorial-cl")))
