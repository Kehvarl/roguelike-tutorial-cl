## Part 0
## Project Setup
### Linux/Ubuntu
* Install Common Lisp (SBCL)
  * `apt install sbcl`

* Install the editor of your choice
  * In this instance I am using Atom
    * https://itsfoss.com/install-atom-ubuntu/
    * I personally prefer to att the PPA and install using apt
  * Atom needs a way to interface with Lisp.  For that I have chosen to use SLIMA
    * https://github.com/neil-lindquist/slima
    * In Atom's Packages Installer, locate and install the following:
      * SLIMA
      * language-lisp
      * lisp-paredit
      * parinfer
      * rainbow-delimiters
  * Atom + SLIMA requires a recent version of SLIME
    * If you have recently installed Emacs, then you probably already have SLIME
    * For this example I am using 2.26.1 from github
    * https://github.com/slime/slime/releases
  * Configure Atom's SLIMA settings with the path to SBCL and your newly downloaded SLIME instance
    * ![SLIMA Settings](./screenshots/part-0-slima-settings.png "SLIMA Settings")
* Install Quicklisp (https://www.quicklisp.org/beta/)
* Install QuickProject `(ql:quickload :quickproject)`
* Create Lisp Project `(quickproject:make-project #p"~/roguelike-tutorial-cl" :depends-on '(cl-blt))`
* Install BeatLibTerminal (https://github.com/tommyettinger/BearLibTerminal)
  * Download and unpack latest release
  * Copy libBearLibTerminal.so to /usr/lib/libBearLibTerminal.so
* Download cl-blt and add to quicklisp local-projects
  * `git clone https://github.com/sjl/cl-blt.git ~/quicklisp/local-projects/cl-blt`
* Symlink your new project into quicklisp's local-projects
  * `ln -s ~/roguelikedev-2021/ ~/quicklisp/local-projects/roguelikedev-2021`
* Attempt to load your package
  * `sbcl` ;; I used `rlwrap sbcl` to give me history in the repl
  * (ql:quickload :roguelikedev-2021)
  * (in-package :roguelikedev-2021)
