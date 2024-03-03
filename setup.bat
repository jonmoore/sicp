@echo off

set "PLTCOLLECTS=%~dp0;"
set "PATH=c:\Program Files\Racket;%PATH%"

@echo on
doskey rkt=racket --require $*
doskey r-enter=racket --repl --eval "(enter! \"$1\")"
doskey r-run=racket --lib errortrace --require $*
doskey r-sicp=racket --eval "(#%%require racket/help racket/trace)"  --lib sicp --repl $*

