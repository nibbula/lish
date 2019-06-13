Lish examples
=============

Many features of Lisp should be familiar from POSIX shells. You can use `|` for
pipes between processes:
```
Lish> echo hello world | wc
 1  2 12
```

A dollar sign in a shell command performs variable substitution
```
Lish> echo My home directory is $HOME
My home directory is /home/user
```

Lish is built on Common Lisp, and evaluates s-expressions:
```
Lish> (+ 1 2 3)
6
```

The results of expressions can be passed as inputs to shell commands
```
Lish> echo (loop for i from 0 below 10 collect i)
0 1 2 3 4 5 6 7 8 9
```

Environment variables
---------------------

Variables are set with `export`, with or without an `=` between the name and the value:
```
Lish> export name "Mister Noodle"
Lish> echo $name
Mister Noodle
```

**Note**: Using single quotes results in:
```
Lish> export name 'Mister Noodle'
WARNING: Extra arguments: (Noodle')
Lish> echo $name
'Mister
```

In Common Lisp, single quotes are used as a prefix to prevent evaluation of
expressions, but only just before and inside parentheses. To avoid confusion
between syntaxes, Lish only uses single quote in the Lisp way. The
un-specialness of single quotes otherwise, can help in avoiding getting tripped
up by English contractions:

```
echo If'n '(+ 2 3) weren't indeed (+ 2 3), 'twould be broke'd.
```

Here the single quote is used in the Lisp way to prevent evaluation of (+ 2 3),
but otherwise use verbatim in commands.

Variables are not split after substitution:
```
Lish> mkdir $name
Lish> ls
Mister Noodle
```
A single directory was created rather than two, which `bash` would have created unless `$name` were quoted.

As in `bash` and many other shells, the `PATH` environment variable is
a string containing a `:` separated list of paths.
These paths are searched from first to last for executables:
```
Lish> echo $PATH
/home/testuser/local/bin:/usr/local/bin:/usr/bin:/bin
```

Environment variables can be accessed inside Lisp expressions using
`nos:env`, which returns a string. For exmaple this string can be split using
the split function.

```
Lish> (split #\: (nos:env "PATH"))
("/home/user/local/bin" "/usr/local/bin" "/usr/bin" "/bin"
 "/usr/local/games" "/usr/games")
```

But note that for this purpose it's probably best to use (nos:command-path-list)
since the layout of the PATH variable differs between operating systems.

A loop over all the entries in `PATH`:
```
Lish> (loop for entry in (nos:command-path-list) do
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
Lish> (setf (nos:environment-variable "TEST") "some string")
Lish> echo $TEST
some string
```
The value must be a string or `nil`, so setting an environment
variable to a number, for example, results in a type error.

Command substitution
--------------------
`!$` returns the lines output from a command as a string of words. This is
basically like `$(command)` or backticks in POSIX shells.

```
Lish> echo In (!$ 'pwd), running (!$ 'uname)
In /home/testuser , running Linux
```

```
Lish> export os (!$ 'uname)
Lish> echo $os
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
Lish> echo hello; echo world
hello
```
Instead you can use `^`, which was chosen since it's mostly unused in other
shells:

```
Lish> echo hello ^ echo world
hello
world
```

Loops
-----

```
Lish> (dotimes (i 5)
- (! "touch file_"  i ".txt")))
NIL

Lish> ls
file_0.txt  file_1.txt  file_2.txt
file_3.txt  file_4.txt
```

But Lish, like some other shells, supports brace expansion. So a simpler way
to do this is:

```
Lish> touch file_{0..5}.txt
```

The `glob` function can be used to get lists of files
and directories:

```
Lish> (loop for file in (glob "*.txt") do
-   (! "cp " file " " file ".bak"))
Lish> ls
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
Lish> opt prompt "[%u@%h %W]\$ "
[user@lisa user]$ 
```

The prompt can also be generated using a function, which takes a shell
as its first argument (but can safely ignore it). Setting
`prompt-function` to `nil` reverts to the default behaviour.
```
Lish> opt prompt-function (lambda (sh) (declare (ignorable sh)) "My prompt> ")
My prompt> 

My prompt> opt prompt-function nil
Lish> 
```

Colors, styles, and evaluated pieces can be put in the `prompt-string`.
For example, here's a fancy prompt showing many features of prompt formatting:

```
opt prompt '((:cyan "%h") ":" (:magenta "%i") ":"
              (:white (:underline "%w")) " "
	      (:red (make-string (1+ *lish-level*) :initial-element #\@))
	      #\space)
```

For the details on thism one can look at the documentation for `format-prompt`
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

One feature of Lish is that Lisp functions and Lish commmands can return
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

These examples utilize "parenless" function calls, which take the pipe output
as the last argument. If the argument you want to pass isn't in the last
position, you can use the variable *input*.

```
Lish> list 9 3 29 24 23 7 | (sort *input* #'<)
```

Note: The corresponding variable *output*, works in shell commands, but not in
normal functions, since *output* always gets set to the return value.
