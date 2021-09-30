# Part 1 - Drawing the @ symbol and moving it around

## Loading our project
We will start with getting the project connected to SLIMA so we have some assistnace from our IDE.
* Open your project folder in Atom.
  * If SLIMA doesn't automatically start, select "Packages" -> "SLIMA" -> "Lisp Process" -> "Start"
  * Once an Atom-Friendly lisp instance is running, it should automatically connect.  If not select "Packages" -> "SLIMA" -> "Lisp Process" -> "Connect".
* At the bottom of your Atom screen you should either have an open REPL, or a pull-out holding that Repl.
* Place your cursor in the REPL after the `CL-USER>` prompt and type the following commands to load your project.
  * `(ql:quickload :rogulike-tutorial-cl)`
  * `(in-package :roguelike-tutorial-cl)`
* If there were any errors, please make sure that you've followed all the steps in part-0, including the symlink into your quicklisp directory.  Also double-check that you've spelled the name of your project correctly.
* ![Loading your project](../screenshots/part-1-0-load-package.png?raw=true "Loading Package in REPL")

## Opening a Window
QuickProject created a main file for us.  In this tutorial that file is named `roguelike-tutorial-cl.lisp`.  If you have a different name for your main file, make note of it and use that anywhere we reference the above filename.

Open your `roguelike-tutorial-cl.lisp` file and ensure it has the following contents:
```lisp
(in-package #:roguelike-tutorial-cl)

(defparameter *game-name* "Common Lisp Roguelike Tutorial")
(defparameter *screen-width* 80)
(defparameter *screen-height* 50)

(defun draw()
  (blt:clear)
  (blt:refresh))

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "window.title = ~A" *game-name*))

(defun main()
  (blt:with-terminal
    (config)
    (loop :do
      (draw)
      (blt:key-case (blt:read)
                    (:escape (return))
                    (:close (return))))))
```

Before we test our code, we need to make our lisp instance aware of it.  Common Lisp doesn't reload the file every time we try to run it, and instead expects we've already applied all our changes to the running system represented by our REPL.

In Atom, we can use the keyboard shortcut `ALT+k` (Sometimes written `M-k`) to "Compile and Load File".  Alternatively, if we've made a change to a single function the command `ALT+c` to "Compile Function".

* Press `ALT+K`
* In your REPL, type `(main)` and press RETURN

![The basic window](../screenshots/part-1-2-blank-window.png?raw=true "Running our project for the first time.")

## Drawing the @ symbol
A blank screen is a boring screen, so now we will place something on it to represent our player.

Edit the `draw` function to match the following
```lisp
(defun draw()
  (blt:clear)
  (setf (blt:color) (blt:white)
        (blt:cell-char 10 10) #\@)
  (blt:refresh))
```

As you can see we've added a couple of new lines. Specifically, we are using `setf` to  set the value of two things.  In fact this could be done as two separate setf calls like so:
```lisp
(defun draw()
  (blt:clear)
  (setf (blt:color) (blt:white))
  (setf (blt:cell-char 10 10) #\@)
  (blt:refresh))
```

This may make it a little more clear what we're setting.
* `(setf (blt:color) (blt:white))` Informs BLT that until otherwise noted, we will draw all characters and symbols in white.
* `(setf (blt:cell-char 10 10) #\@)` Tells BLT to update the character at cell 10,10 to our new symbol "@".   In Common Lisp, #\@ refers to the @ character itself instead of to a string containing that character.

* Press `ALT+c` (Compile function)
* In your REPL, type `(main)` and press RETURN

![The first @](../screenshots/part-1-3-first-@.png?raw=true "Drawing a symbol on our screen.")

## Keyboard Input

In addition to providing the terminal, BLT can monitor the keyboard for user input for us.  It's actually already doing that in our existing demo:
```lisp
(blt:key-case (blt:read)
  (:escape (return))
  (:close (return)))
```
This fragment does a few things:
* First, we make a call to `(blt:read)` to get any currently pressed keys
* Next we pass that result to `(blt:key-case` which is a `case` statement specifically for handling BLT's key events.
* `blt:key-case` then compared the currently pressed key to our various tests until either one passes or all fail. (eg: `:escape` which represents the ESC key)
* If one test case passes, the expression after the test is evaluated. (eg: `(return)` to exit our current function).

While it was convenient enough to check for the exit key in our main function, let's move that our to its own `handle-keys` function so we can modify it later.

```Lisp
(defun handle-keys ()
  (let ((action nil))
    (blt:key-case (blt:read)
                  (:escape (setf action (list :quit t)))
                  (:close (setf action (list :quit t))))
    action))
```

Remember to press `ALT+c` to compile our new function

You will have noticed that this function doesn't just call `(return)` when we hit ESC, instead we return a list containing 2 values:  The keyword `:quit` and the value `t`.  We will modify our `main` function to work with this returned message.

```lisp
(defun main()
  (blt:with-terminal
    (config)
    (loop :do
         (draw)
         (let* ((action (handle-keys))
                (exit (getf action :quit)))
           (if exit
             (return))))))
```

Be sure you press `ALT+c` to compile our changed function, or `ALT+k` to compile and load the entire file.

Here we have expanded our main loop.  We draw our terminal, then check for a keypress.  If `handle-keys` returns a message of `(:quit t)` then  we exit the loop which nicely terminates the program

Specifically what we are doing is:
* `(let*`  Bind some variables.  the * version of `let` allows the variables to reference each other.
*  `(action (handle-keys))`  Assign the output of `handle-keys` to the variable `action`
* `(exit (getf action :quit))` If our `action` variable contains the `:quit` keyword, get it's value and assign it to our new variable `exit`.  If not then assign `nil` to `exit`.
* `(if exit (return))` If our `exit` variable is set to `t`, then exit our loop which will end the program.


## Moving Around
