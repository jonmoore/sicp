@echo off
powershell -ExecutionPolicy bypass -file %~dp0\replace.ps1 %1 %2
