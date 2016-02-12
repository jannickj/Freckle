@echo off
cls

.paket\paket.bootstrapper.exe
if errorlevel 1 (
  exit /b %errorlevel%
)

.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

IF NOT EXIST config.fsx (
  packages\build\FAKE\tools\FAKE.exe init.fsx
)
packages\build\FAKE\tools\FAKE.exe build.fsx %*
