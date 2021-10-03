# Part 2 - The generic Entity, the render functions, and the map

Now we have a character to move around, but nothing really to move them around in.  However, before we dive into creating a dungeon and map, let's take a moment to think about the player character itself.

Currently we just track the X and Y coordinates, and let the `(draw)` function put our chosen symbol in place. It might be better if we had the important parts of the Player character kept together in one place, possibly with a routine to draw them wherever we like. Enter...

## The Generic Entity
The Player character is really just one of many things we will need to have on the screen at any given time.  For the sake of convenience, we'll represent them with a class (if you're unfamiliar with classes in Common Lisp, take a look at the CLOS section of [The Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/clos.html)).  

Specifically, this class will (initially) help us keep a record of:  
* The Entity's positon (X and Y coordinates)
* The Entity's appearance (What symbol to display, and what color to use)

I'm certain you can come up with any number of other useful pieces of information to track for the entities, but we'll keep this as simple as possible for now.

### The Entity Class
Still working in our main file for now, let's create the class for tracking our entities:
```lisp
(defclass entity ()
  ((x :initarg :x :accessor entity/x)
   (y :initarg :y :accessor entity/y)
   (char :initarg :char :accessor entity/char)
   (color :initarg :color :accessor entity/color)))
```
Here we define a new class named `entity`, inheriting from no other classes, and then we lay out what slots it has for us to store information in.
* `(defclass entity ()` Create a class named `entity` with no parent class.
* `((x ...)` `entity` will hold a variable `x`.  
  * `:initarg :x` When we create a new `entity`, we can use the `:x` keyword to set this slot's initial value.
  * `:accessor entity/x` We will also define a function to access the entity's `x` variable to set or read it at times other than creation.   
* The other slots are defined identically to `x`.

Now that we have a class, we can give it methods.  In the Common Lisp Object System (CLOS), methods are defined a little differently than you'll see in most languages.  We might define a generic version of the method, and them any number of implementations depending on the precise classes that it is intended to use.

```lisp
(defmethod move ((e entity) dx dy)
  (incf (entity/x e) dx)
  (incf (entity/y e) dy))
```

This isn't a terribly complicated class, so it's a good starting point:
* `(defmethod move ((e entity) dx dy)` Create a method `move` that expects an entity which we will refer to internally as `e`, and two other parameters we'll call `dx` and `dy` (for "delta" or "change-in" X and Y).
* `(incf (entity/x e) dx)` Our method uses the good old `incf` function we encountered before, but we're using it interestingly.
  * `(entity/x e)` Instead of a variable, we're giving `incf` the accessor function for the variable X within an Entity, and we're passing our entity to that accessor.  In essence, we're asking our entity where it keeps its X value and telling `incf` to use that location.
* `(incf (entity/y e) dy))`  We repeat our `incf` except with the entity `y` value and our `dy` parameter.

We need another method for our entity:  A generic way to draw any entity we want.
```lisp
(defmethod draw ((e entity))
  (with-slots (x y char color) e
    (setf (blt:color) color
          (blt:cell-char x y) char)))
```
The  only really new thing here is how we access the object that's given to us.
* `(with-slots (x y char color) e` The with-slots macro saves us from using an accessor for every single slot we want to access.   Without it we would have to say something like: `(blt:cell-char (entity/x e) (entity/y e)) ) (entity/char e))`.


This time when you press `ALT+c` you'll encounter an error!
![The Debug Screen](../screenshots/part-2-1-Debug.png?raw=true "The SLIME/SLIMA debug interface")

There's nothing to worry about.  A quick reading of the debug warning tells us that we're trying to define something named `draw`, but something else in our package is already using that name.

Fortunately, it's an easy fix.  We'll click `Continue` to remove everything named `draw` that's currently in memory, then `ALT+c` in our new method one more time.  And this time with no errors or warnings!

Of course, now we have that old `draw` function that we just removed from the running system.  Its implementation is still sitting in our file, and we can actually make use of it:
```lisp
(defun render-all (entities)
  (blt:clear)
  (mapc #'draw entities)
  (blt:refresh))
```

As you can see, we've renamed our old `draw` function `render-all`, and changed some things about it:
* `(defun render-all (entities)`  Instead of our player's position, we expect a list of Entity instances
* `(mapc #'draw entities)` Is deceptively simple and powerful:
  * `mapc` - the Common Lisp "map": command:  Apply the given function to each item in a list
  * `#'draw`  -The function to call.  mapc expects a function, not a function name, so we use the sharp-quote #' format to get the function named draw for mapc to use.
  * `entities` - The list containing the items (in this case instances of the entity class) to which we will apply our function.


One last change and we'll run the game again.  This one is fairly large, and introduces some new concepts, but it's not too complicated once we break it down:
```lisp
(defun main()
  (blt:with-terminal
    (config)
    (loop
      :with player = (make-instance 'entity
                                    :x (/ *screen-width* 2)
                                    :y (/ *screen-height* 2)
                                    :char #\@
                                    :color (blt:white))
      :and  npc = (make-instance 'entity
                                 :x (- (/ *screen-width* 2) 5)
                                 :y (/ *screen-height* 2)
                                 :char #\@
                                 :color (blt:yellow))
      :with entities = (list player npc)
      :do
        (render-all entities)
        (let* ((action (handle-keys))
               (move (getf action :move))
               (exit (getf action :quit)))

          (if exit
            (return))

          (when move
            (move player (car move) (cdr move)))))))
```
Our first change is to remove the `player-x` and `player-y` variables.  Instead we have this block:

* `(make-instance 'entity` Create an instance of the `entity` class.  We need to "quote" the class name since we want it's name, and not whatever is stored in the variable "entity".
* `:x (/ *screen-width* 2)` Our `entity` class accepts a `:x` initarg which it uses to set the `x` slot.  In this case to half the screen width.
* `:y (/ *screen-height* 2)` Set the `y` slot to half the screen height (Thus centering our Player Character on the screen).
* `:char #\@` We're still using the "@" character for our player
* `:color (blt:white))` And we want to display the character in white.


We've also defined an NPC character in exactly the same fashion, allowing us to display two entities on the screen.

* `:with entities = (list player npc)` It doesn't make sense to work with individual entities, so we store them in a list that we can reference as a group.

* `(render-all entities)` Instead of drawing just the player, we'll use that nice `render-all` function we created and give it our list of entities to draw every round.

* `(when move (move player (car move) (cdr move)))` We also have a useful `move` method for our entities, so when the player presses a movement key, we can just pass the player's entity and the desired change in location to `move` and let it handle interacting with our object.
  * If you thought that this would break because we have a variable named `move` and a function named `move`, then welcome to another of Lisp's surprises.  If staring at this makes you deeply uncomfortable, feel free to rename things (perhaps `action-move` for the `:move` result from `handle-keys` returned `action`?)

![Dynamic Drawing!](../screenshots/part-2-4-two-entities.gif?raw=true "Achievement Unlocked: Drawing an arbitrary number of entities.")

### Cleaning up our Main file
Before we move on to the map, let's do some cleanup.  Having everything in one file is fine for testing and prototyping, but it will quickly become cluttered.  To help manage that clutter, we're going to move all of our new entity-related code to an `entity.lisp` file.

* Start by creating a new file and naming it `entity.lisp`.
* The first line needs to be `(in-package #:roguelike-tutorial-cl)` to inform Lisp that the contents of this file belong in our project's package.
* Next up we will move the parts of `entity` out of our main file and into the new file:
  * ```lisp
  (defclass entity ()
    ((x :initarg :x :accessor entity/x)
     (y :initarg :y :accessor entity/y)
     (char :initarg :char :accessor entity/char)
     (color :initarg :color :accessor entity/color)))

  (defmethod move ((e entity) dx dy)
    (incf (entity/x e) dx)
    (incf (entity/y e) dy))

  (defmethod draw ((e entity))
    (with-slots (x y char color) e
      (setf (blt:color) color
            (blt:cell-char x y) char)))
    ```

* We'll also update the `roguelike-tutorial-cl.asd` file to tell Lisp what order to load files in when we load the package.
  * ```lisp
  :components ((:file "package")
               (:file "entity")
               (:file "roguelike-tutorial-cl")))
  ```

Don't forget to compile and load all your changes, and then run the project to make sure everything still works.
