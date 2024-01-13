@echo off

REM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
REM   Point your CMD startup command to this script.;;
REM :::::::::::::::::::::::::::::::::::::::::::::::;;;

set path=w:\bin;%path%
call "C:\Program Files\Microsoft Visual Studio\2022\Community\Common7\Tools\VsDevCmd.bat" -startdir=none -arch=x64 -host_arch=x64
