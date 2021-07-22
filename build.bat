@echo off

rem This is only for SBCL currently.
rem Also it doesn't work unless you've already done a grovelling compile run
rem under cygwin or something, and a platform appropriate libffi.dll is in the
rem path.
rem
rem There are multiple difficult problems:
rem  1. Make implementations be able to call by struct value on supported
rem     platforms. CCL can call by struct value, but sbcl can't.
rem  2. Make CFFI be able to use the implmentation's call by struct value
rem     without libffi on supported implementations.
rem  3. Replace whatever grovels with a non-grovelling version.
rem     Either:
rem     - Replace whatever it is by my version which uses opsys
rem     - Make a patched version of whatever by replacing grovelling with
rem       opsys calls and maintain a fork.
rem 
rem I suppose a workaround could be if we supply a libffi.dll and the groveller
rem output and copy them in to place here?
rem
rem Another problem is that the compiling doesn't flush the output enough, so
rem we don't see progess as it happens. This seems like no too hard to fix.

rem set LISP=sbcl
set LISH_PLAIN_FLAGS=--no-userinit
set LISH_FLAGS=--noinform --noprint --disable-debugger --no-sysinit
set TARGET=lishpfu

rem echo "[Using %LISP% %LISH_FLAGS% %LISH_PLAIN_FLAGS% ]"

rem %LISP% %BATCH_ARGS% %LISH_PLAIN_FLAGS% --load build/build-lish.lisp

rem sbcl --noinform --noprint --no-userinit --disble-debugger -no-sysinit --load build/build-lish.lisp
sbcl --noinform --noprint --no-userinit --disable-debugger --no-sysinit --load "build/build-lish.lisp"
