rem run "raco test" on all sub-directories beginning with "rackunit".

rem Setting PLTCOLLECTS is not required if invoking "raco test" from
rem this directory.  However it is required if invoking it from
rem another directory, e.g. from the parent with "raco test
rem sicp\rackunit"

rem The ; path separator at the end is needed if setting this. Also
rem from the manual: "If you set PLTCOLLECTS, include an empty path in
rem by starting the value with a colon (Unix and Mac OS) or semicolon
rem (Windows) so that the original search paths are preserved."
rem
rem set "PLTCOLLECTS=%CD%\sicp;"

rem Set the directories to test.  This cmd hackery can be ignored.
@echo off
set rackunit_dirs=
for /f "tokens=*" %%F in ('dir /b /a:d "rackunit*"') do call set rackunit_dirs=%%rackunit_dirs%% %%F
@echo on

raco test %rackunit_dirs%
