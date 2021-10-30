#Part 5 - Placing enemies and kicking them (harmlessly)

## Placing Enemies
While we could come up with some complicated way to decide what enemies to place where, we'll start by placing a few randomly around the map.

First up, we a way to check for an entity already in a location, that way we don't stack them atop each other.   Over in our `game-map`, let's add a new function:
```lisp
(defun entity-at (entities x y)
  (dolist (entity entities)
    (if (and (= (entity/x entity) x)
             (= (entity/y entity) y))
      (return entity))))
```
This simply loops through the list of entities and returns the first one where the `x` and `y` positions match the target position.  In fact, it doesn't even use our map, and we could probably make it part of the `entity` file or even our `entity` class.  For now, it is fine in `game-map` where we'll be using it.
