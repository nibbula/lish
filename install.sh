#!/usr/bin/env sh
#
# install.sh - Install Lish
#

# This is really just a stupid ‘cp’ command. You can just ‘cp’ lish to your bin.
#
# @@@ We could write this in lish, like the build script, but unfortunately
# we'd need to make sure "los" is around and/or loaded, for "mkdir -p" and
# "cp" and maybe "really", or make ½-assed versions of them. It doesn't
# actually help much with portability, since non-unix OS's will likely be
# totally different.
#
# @@@ Someday convert to lish, since we at least should have the lish executable
# and be able to use that even if it doesn't have los!

fail()
{
  echo "$@"
  echo "Installing Lish failed. Sorry."
  exit 1
}

program_name=${LISH:=lish}
prefix=${PREFIX:=/usr/local/}
destination="${PREFIX}/bin/"
try_sudo=

if [ -z "$program_name" ]; then
  fail '$LISH is not set or is empty.'
fi
if [ ! -e "$program_name" ]; then
  fail "$program_name doesn't exist."
fi
if [ ! -f "$program_name" ]; then
  fail "$program_name isn't a regular file."
fi
if [ ! -x "$program_name" ]; then
  fail "$program_name isn't executable."
fi

if [ ! -w "$destination" ]; then
  if [ "$1" = '-i' ]; then
    echo "The destination $destination is not writable."
    if type sudo > /dev/null ; then
      echo "Trying sudo"
      try_sudo=sudo
    elif type doas > /dev/null ; then
      echo "Trying doas"
      try_sudo=doas
    elif type su > /dev/null ; then
      echo "Trying su"
      try_sudo=su
    else
      echo "I can't find a way to get root."
    fi
  else
    fail "The destination $destination is not writable. \
Maybe be root or try -i ?"
  fi
fi

if [ ! -d "$destination" ]; then
  printf "Making $destination..."
  $try_sudo mkdir -p "$destination" || \
    fail "Can't make the destination. Maybe you need root?"
  echo "OK"
fi

new_name="${destination}/${program_name}"
if [ -f "${new_name}" ] ; then
  printf "Trying to rename current $program_name in $destination..."
  $try_sudo mv "${new_name}" "${new_name}_bak" || \
    fail "Couldn't rename ${new_name} to ${new_name}_bak. Maybe you need root?"
  echo "OK"
fi

printf "Copying $program_name to $destination..."
$try_sudo cp "$program_name" "$destination" || \
  fail "Couldn't copy $program_name to $destination. Maybe you need root?"
echo "OK"

echo "Install succeeded."
exit 0
