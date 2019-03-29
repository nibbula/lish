##
## Makefile - fake makefile for lish
##

# This is just so that just saying 'make' can work.

default:
	sh ./build.sh

install:
	sh ./build.sh install

clean:
	sh ./build.sh clean

.PHONY: default install clean
