# Part 3 - Generating a dungeon
 In the previous part we manually created a wall, and left the rest of the map open.  In this part we will perform some experiments that culminate in randomly generating a dungeon every time we start a new game!

## Changing Some Defaults
Instead of starting with an empty map and filling it with walls, we will start with a map completely made of walls and then carve our some open spaces to play in.
```lisp
(dotimes (y (game-map/h map))
  (dotimes (x (game-map/w map))
     (setf (aref (game-map/tiles map) x y) (make-instance 'tile :blocked t))))
```
This was a one-line change.   Specifically, we changed our `make-instance` and added the initarg `:blocked t` to initialize all tiles as `blocked` when we create a map.

If you compile this change and run your code, the player character will now be trapped in one place forever.

![All walls and nowhere to go](../screenshots/part-3-1-all-walls.png?raw=true "Instant Claustrophobia")
