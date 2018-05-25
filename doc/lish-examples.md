Lish examples
=============

Some examples of Lish use, probably misguided or incorrect.
Any good parts were probably adapted from the [Fish tutorial](https://fishshell.com/docs/current/tutorial.html)
and docs 

Some features of Lisp should be familiar from POSIX shells, including use of
`|` for pipes between processes:
```
$ echo hello world | wc
 1  2 12
```

A dollar sign in a shell command performs variable substitution
```
$ echo My home directory is $HOME
My home directory is /home/testuser
```

Lish is built on Common Lisp, and evaluates s-expressions:
```
$ (+ 1 2 3)
6
```

The results of expressions can be passed as inputs to shell commands
```
$ echo (loop for i from 0 below 10 collecting i)
0 1 2 3 4 5 6 7 8 9
```

Environment variables
---------------------

Variables are set with `export`, with or without an `=` between the name and the value:
```
$ export name "Mister Noodle"
$ echo $name
Mister Noodle
```

**Note**: Using single quotes results in:
```
$ export name 'Mister Noodle'
WARNING: Extra arguments: (Noodle')
$ echo $name
'Mister
```

Single quotes are instead used in the typical Lisp way of preventing evaluation
of expressions, but only just before and inside parentheses. The un-specialness
of single quotes otherwise, can help in avoiding getting tripped up by English
contractions:

```
echo If'n '(+ 2 3) weren't indeed (+ 2 3), 'twould be broke'd.
```

Variables are not split after substitution:
```
$ mkdir $name
$ ls
Mister Noodle
```
A single directory was created rather than two, which `bash` would have created unless `$name` were quoted.

As in `bash` and many other shells, the `PATH` environment variable is
a string containing a `:` separated list of paths.
These paths are searched from first to last for executables:
```
$ echo $PATH
/home/testuser/local/bin:/usr/local/bin:/usr/bin:/bin
```

Environment variables can be accessed inside Lisp expressions using
`nos:environment-variable`, which returns a string. This string can be split
using the split function.

```
$ (split #\: (nos:environment-variable "PATH"))
("/home/testuser/local/bin" "/usr/local/bin" "/usr/bin" "/bin"
 "/usr/local/games" "/usr/games")
```

A loop over all the entries in `PATH`:
```
$ (loop for entry in (split #\: (nos:environment-variable "PATH")) do
- (format t "entry: ~a~%" entry))

entry: /home/testuser/local/bin
entry: /usr/local/bin
entry: /usr/bin
entry: /bin
entry: /usr/local/games
entry: /usr/games
NIL
```

The environment variable locations are also setf'able:
```
$ (setf (nos:environment-variable "TEST") "some string")
$ echo $TEST
some string
```
The value must be a string or `nil`, so setting an environment
variable to a number, for example, results in a type error.

Command substitution
--------------------
`!$` returns the lines output from a command as a string of words. This is
basically like `$(command)` or backticks in POSIX shells.

```
$ echo In (!$ 'pwd), running (!$ 'uname)
In /home/testuser , running Linux
```

```
$ export os (!$ 'uname)
$ echo $os
Linux
```

There are a number command substitution functions, depending on how you want
the output. For convenience they are named with very short names, starting with
`!`. These functions aren't special or part of the syntax. You can define your
own command substitution functions to suit your needs.

For instance, `!_` turns the output in to a list of lines.

```
echo You have (count-if (_ (begins-with "??" _)) (!_ "git status --porcelain")) untracked files.
```

`!@` spreads the entire output into separate words, instead of one word.
So,
```
touch (!$= "echo" "foo bar")
```
creates one file named "foo bar", but
```
touch (!@= "echo" "foo bar")
```
creates two files named "foo" and "bar".


Separating commands
-------------------

Lish uses lisp style comments, which begin with `;`.
The usual way to separate commands therefore doesn't work:

```
$ echo hello; echo world
hello
```
Instead you can use `^`:

```
$ echo hello ^ echo world
hello
world
```

Loops
-----

```
$ (dotimes (i 5)
- (! "touch file_"  i ".txt")))
NIL

$ ls
file_0.txt  file_1.txt  file_2.txt
file_3.txt  file_4.txt
```

The `glob` function can be used to get lists of files
and directories:

```
$ (loop for file in (glob "*.txt") do
-   (! "cp " file " " file ".bak"))
$ ls
file_0.txt      file_2.txt.bak
file_0.txt.bak  file_3.txt
file_1.txt      file_3.txt.bak
file_1.txt.bak  file_4.txt
file_2.txt      file_4.txt.bak
```

Prompt
------

The default prompt is configured using a bash-style prompt string.
Like other user-configurable Lish options, it can be set using the
`opt` built-in command. See `help opt` for a list.
```
$ opt prompt "[%u@%h %W]\$ "
[testuser@lisa testuser]$ 
```

The prompt can also be generated using a function, which takes a shell
as its first argument (but can safely ignore it). Setting
`prompt-function` to `nil` reverts to the default behaviour.
```
$ opt prompt-function (lambda (sh) (declare (ignorable sh)) "My prompt> ")
My prompt> 

My prompt> opt prompt-function nil
$ 
```

Colors, styles, and evaluated pieces can be put in the `prompt-string`.
For example, here's a fancy prompt showing many features of prompt formatting:

```
opt prompt '((:fg-cyan "%h") ":" (:fg-magenta "%i") ":"
              (:fg-white (:underline "%w")) " "
	      (:fg-red (make-string (1+ *lish-level*) :initial-element #\@))
	      #\space)
```

For the details on this one can look at the documentation for `format-prompt`
and `symbolic-prompt-to-string` which can usually be dredged up by the standard
Lisp functions `describe` or `documentation`, e.g.:

```
(describe 'format-prompt)
```
and

```
(documentation 'symbolic-prompt-to-string 'function)
```

Lisp functions
--------------
In a previous exmaple, we had shown how to evaluate Lisp s-expressions
(a.k.a expressions in parentheses), but Lish can also call Lisp functions
without parentheses:

```
Lish> lisp-implementation-version
"1.3.7.62-a2d1969-dirty"
Lish> sin pi
1.2246467991473532d-16
```

Although note if you something like say:
```
Lish> sleep .5
Lish> ; delayed by 1/2 a second.
```

you will probably get the system sleep command, instead of the Common Lisp
sleep function. You can tell which one you might get first by using the `type`
command, like in other shells.

```
Lish> type -a sleep
sleep is /bin/sleep
sleep is the function #<FUNCTION SLEEP>
```

One quirky feature of Lish is that Lisp functions and Lish commmands can return
objects, and those objects are passed to subsequent functions in a pipeline.
If that command happens to be a Lisp function with at least one less argument
than it expects, that argument is taken from the result of the previous command
in pipeline:

```
Lish> get-universal-time | decode-universal-time
50 ;
9 ;
4 ;
25 ;
5 ;
2018 ;
4 ;
T ;
8
Lish> sin pi | expt 2
1.0d0
Lish> (expt 2 (sin pi)) ; Same as the last line, but in s-exp style.
1.0d0
Lish> glob "/bin/d*" | mapcar 'reverse | mapcar 'string-capitalize
```

> I'd like to come up with more useful, interesting, or practical examples.
> Unfortunately I don't use this feature much myself, as I'm so used to s-exps.
> But I know not everyone finds this an easy way to say
> "show me the top 10 linked files in root":
```
(format t "~{~a~%~}~%" (subseq (sort (mapcar (_ (cons (uos:file-status-links (car _)) (cdr _))) (mapcar (_ (cons (uos:stat _) _)) (glob "/*"))) #'> :key 'car) 0 10))
```

> This is probably quite a bit too much for an introduction.

A little spell checker:

```
(defvar *dict* (!_ "cat /usr/share/dict/words"))
mapcar (_ (split #\  _)) (!_ "cat README.md") |
flatten | mapcar 'string-downcase |
mapcar (_ (remove-if (_ (and (char/= #\' _) (not (alpha-char-p _)))) _)) |
remove-if (_ (or (zerop (length _)) (member _ *dict* :test #'equalp))) |
funcall (_ (remove-duplicates _ :test #'equal))
```
Perhaps because I have too much nostalgia for [this][https://www.youtube.com/watch?v=tc4ROCJYbm0]
