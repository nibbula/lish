How the build works.

0 - ../build.sh
    Bootstrap POSIX shell script.
    Picks a Lisp to run and runs it loading "build-lish.lisp".

1 - build-lish.lisp
    Lisp "Makefile".
    a. loads "horrible.lisp"
       Some minimal functions we need to do anything.
    b. loads "load-asdf.lisp"
       Loads ASDF.
    c. Picks a target to build.
       One of:
       - lish
         Normal lish with whatever the current setup is.
       - lishfu
       	 Fully loaded lisp, with los and stuff from the town.
       - lishp
       	 Plain lish, without user startup and only the minimal things
	 needed to build.

       Each build target:
         - Runs in a separate Lisp
	 - Loads "build-init.lisp"
	   0 - loads "horrible.lisp"
	   1 - loads "load-asdf.lisp"
	   2 - loads "load-quicklisp.lisp" which tries to load QuickLisp.
	       If QuickLisp isn't already installed, or a custom one isn't
	       given in LISP_QUICKLISP, then we install one. This is totally
	       insecure, and I'm really sorry.
	       Note that "quicklisp.lisp" is 'vendored' here.
	   3 - Sets up ASDF to find things.
	 - Loads whatever pre-requirements, which vary depending on the target.
	 - Loads lish
	 - Saves an executable

If you want building to increment the build version number, set the environment
variable LISH_MAINTAINER. This is probably only useful if you are publishing it.
