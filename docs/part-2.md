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
