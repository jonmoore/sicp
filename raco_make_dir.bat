@echo off

setlocal

set target_dir="%~1"
set racket_files=
for /f "tokens=*" %%F in ('dir /b /a:-d "%target_dir%\*.rkt"') do call set racket_files=%%racket_files%% "%%F"

cd %target_dir%
raco make %RACO_MAKE_FLAGS%  %racket_files%

endlocal
