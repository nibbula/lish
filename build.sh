#!/usr/bin/env sh
#
# build.sh - Build Lish
#

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

if [ -n "$1" ]; then
  export TARGET="$1"
fi

#${SHELL:-sh} ./run_lisp.sh "build/build-lish.lisp" || fail
/usr/bin/env sh ./run_lisp.sh "build/build-lish.lisp" || fail

exit 0
