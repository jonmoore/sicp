# ;;; -*- mode: org; -*-

This directory is used to define a custom language for SICP work.

It's based on https://github.com/v--/sicp. As that notes, "custom
language" is a bit of an overstatement, it's simply Racket with
additional global definitions.

To aid debugging and to avoid confusion with the "sicp" language
available through Racket, I have renamed it "sicp2".

In lang/reader.txt I'm unsure if it is necessary to give the language
the same name ("sicp2") as the containing directory, but this seems
sensible.

To use this
- Set PLTCOLLECTS to include the parent directory, e.g. 
#+BEGIN_EXAMPLE
rem The ; path separator at the end is needed if setting this. 
set "PLTCOLLECTS=%CD%\sicp;"
#+END_EXAMPLE
- Set the language at the top of your source file, e.g. 
#+BEGIN_EXAMPLE
#lang sicp2

(define (double x)
  (* x 2))

;; ...
#+END_EXAMPLE
