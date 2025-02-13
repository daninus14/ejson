@setlocal

@set SRC_DIR=%~dp0..\src\
@set BIN_DIR=%~dp0bin\
@set EJSON_ASD=%SRC_DIR%ejson.asd
@set EJSON_EXE=%BIN_DIR%ejson-parsing.exe

@rem Escape backslashes for SBCL string literals
@set EJSON_ASD_E=%EJSON_ASD:\=\\%
@set EJSON_EXE_E=%EJSON_EXE:\=\\%

@set BUILD_EXP=^
(sb-ext:save-lisp-and-die \"%EJSON_EXE_E%\"^
  :toplevel (lambda ()^
              (handler-case (sb-ext:exit :code (apply #'ejson-parsing:main sb-ext:*posix-argv*))^
                (error ()^
                  (sb-ext:exit :code 2 :abort t))^
                (sb-sys:interactive-interrupt ()^
                  (sb-ext:exit :code -1073741510 :abort t))))^
  :executable t)

@mkdir "%BIN_DIR%"
@sbcl^
 --noinform^
 --end-runtime-options^
 --no-sysinit^
 --no-userinit^
 --disable-debugger^
 --eval "(load """"~/quicklisp/setup.lisp"""")"^
 --eval "(asdf:load-asd #p""%EJSON_ASD_E%\"")"^
 --eval "(ql:quickload '#:ejson)"^
 --eval "(ql:quickload '#:cl-json)"^
 --eval "(ql:quickload '#:jonathan)"^
 --eval "(ql:quickload '#:json-streams)"^
 --eval "(ql:quickload '#:jsown)"^
 --eval "(ql:quickload '#:shasht)"^
 --eval "(ql:quickload '#:yason)"^
 --load "%~dp0ejson-parsing.lisp"^
 --eval "%BUILD_EXP%"
@if %errorlevel% neq 0 exit /b %errorlevel%

@endlocal
