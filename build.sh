#!/bin/sh

# This is just to find and start a Lisp, to run the Lisp based build script.

if [ ! -n "$LISP" ] ; then
  LISPS="sbcl ccl lisp clisp ecl"
  for l in $LISPS ; do
    if [ x`command -v $l` != x ] ; then
      LISP=$l
      break;
    fi
  done
fi
if [ ! -n "$LISP" -o ! -n `command -v $LISP` ] ; then
  echo "I can't find a Lisp to run. Please set the environment variable LISP to"
  echo "the name of an installed Common Lisp, and re-run this script."
  echo "For example: "
  echo
  echo "LISP=/usr/bin/sbcl sh build.sh"
  echo
  exit 1
fi

try_to_figure_flags()
{
  case "$LISP" in
    *sbcl*)  OUR_PLAIN_LISP_FLAGS="--no-userinit" ;
	     BATCH_ARGS="--noinform --noprint --disable-debugger --no-sysinit"
	     ;;
    *ccl*)   OUR_PLAIN_LISP_FLAGS="--no-init"     ; BATCH_ARGS="" ;;
    *clisp*) OUR_PLAIN_LISP_FLAGS="-norc"         ; BATCH_ARGS="" ;;
    *abcl*)  OUR_PLAIN_LISP_FLAGS="--noinit"      ; BATCH_ARGS="" ;;
    *ecl*)   OUR_PLAIN_LISP_FLAGS="--norc"        ; BATCH_ARGS="" ;;
    *)
      echo "I'm not sure how to set flags for $LISP."
      ;;
  esac
}

fail()
{
  echo
  echo "Building Lish failed. Sorry."
  echo
  echo "/-------------------------------------------------------------------\\"
  echo
  echo "  If it failed for some missing requirement, it should say so"
  echo "  somewhere above."
  echo
  echo "  If it failed due to something in an initialization file, you can"
  echo "  use the '-p' option, to try building without loading the"
  echo "  initialization file. For example:"
  echo
  echo "sh build.sh -p"
  echo
  echo "  Or you may need to set PLAIN_LISP_FLAGS to command line arguments to"
  echo "  run Lisp without loading the initialization file. For example:"
  echo
  echo "PLAIN_LISP_FLAGS=--no-init sh build.sh"
  echo
  echo "\\-------------------------------------------------------------------/"
}

try_to_figure_flags
export LISH_PLAIN_FLAGS="${LISH_PLAIN_FLAGS:=$OUR_PLAIN_LISP_FLAGS}"
export LISH_FLAGS="${BATCH_ARGS}"

echo "[Using ${LISP} ${LISH_FLAGS} ${LISH_PLAIN_FLAGS}]"

echo '(load "build/build-lish.lisp")' |
  $LISP $LISH_FLAGS $LISH_PLAIN_FLAGS "$@" || fail

exit 0
