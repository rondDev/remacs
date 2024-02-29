:: Forward the ./rmcs script to Emacs

@ECHO OFF
SETLOCAL ENABLEDELAYEDEXPANSION

PUSHD "%~dp0" >NUL

SET args=
SET command=%1

:LOOP
SHIFT /1
IF NOT [%1]==[] (
    SET args=%args% %1
    GOTO :LOOP
)

IF [%command%]==[run] (
   start runemacs -Q %args% -l ..\init.el -f "rmcs-run-all-startup-hooks-h"
) ELSE (
   emacs --quick --script .\rmcs -- %*
)

POPD >NUL
ECHO ON
