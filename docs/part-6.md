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

```
