#Part 6 - Doing (and taking) some damage
So far we have created a window, figured out how to draw on it, created a random map, created a generic entity type which we can place anywhere on the map, and added turns for the player and the other entities.

Now we will add some interaction by making it possible to attack those other entities and do some damage to them (and eventually allowing those monsters to do damage right back and move around on their own).

## Components
The key to our new goal is the Component.  A component is a feature that our entities might have, but which not all entities actually have.  This allows us to use "composition" to build our entities out of the bits and pieces we want while giving us the flexibility to have only those pieces we want.

### New file
We will start with creating a new file named `components.lisp`  Don't forget to `in-package` it and add it to our .asd

For starters we'll create a generic `component` class that our other components can inherit from, so we can give them some consistency:
```lisp
(defclass component ()
  ((owner :initarg :owner :accessor component/owner)))
```
No new concepts here, but this is the core of our entire new composition system.

Next up we will define the first component we'll actually use:
```lisp
(defclass fighter (component)
  ((max-hp :initarg :max-hp :accessor fighter/max-hp :initform nil)
   (hp :initarg :hp :accessor fighter/hp)
   (defense :initarg :defense :accessor fighter/defense)
   (power :initarg :power :accessor fighter/power)))
```

Let's also create a component to represent AI behavior and a take-turn method for it:
```Lisp
(defclass basic-monster (component) ())
date
man date

qsudo date 111117562021
sudo apt uupdatesudo apt upgraded-array-element-typeps -ae

(defgeneric take-turn (component))
(defmethod take-turn ((component basic-monster))
  (format t "The ~A wonders when it will get to move.~%" (component/owner component)))
```

Let's set up our entity to handle components:
```lisp
(defclass entity ()
  ((name :initarg :name :accessor entity/name)
   (x :initarg :x :accessor entity/x)
   (y :initarg :y :accessor entity/y)
   (char :initarg :char :accessor entity/char)
   (color :initarg :color :accessor entity/color)
   (blocks :initarg :blocks :accessor entity/blocks :initform nil)
   (fighter :initarg :fighter :accessor entity/fighter :initform nil)
   (ai :initarg :fighter :accessor entity/ai :initform nil)))
```
Here we've added 2 new slots to our entity, and initialized them to `nil`.  We'll also create an after-initialize method to make components more useful:

```lisp
(defmethod initialize-instance :after ((entity entity) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fighter ai) entity
    (when fighter
      (setf (component/owner fighter) entity))
    (when ai
      (setf (component/owner ai) entity))))
```

Here, when we create an entity, if we've set a fighter or ai component, we make sure to initialize those component's `owner` reference to the entity we created.  We can use this later to test for things and make changes.
