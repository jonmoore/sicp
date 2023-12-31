@echo off

set "PLTCOLLECTS=%~dp0;"
set "PATH=c:\Program Files\Racket;%PATH%"

doskey te=raco test -t $*
doskey tr=racket -l errortrace -t $*
doskey rkt=racket -t $*
doskey sicp=racket -e "(#%%require racket/help racket/trace)"  -l sicp -i $*
