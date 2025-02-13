@setlocal

@set SRC_DIR=%~dp0src\
@set TEST_DIR=%~dp0test\
@set EJSON_ASD=%SRC_DIR%ejson.asd
@set EJSON_TESTS_ASD=%TEST_DIR%ejson-tests.asd

@rem Escape backslashes for SBCL string literals
@set EJSON_ASD_E=%EJSON_ASD:\=\\%
@set EJSON_TESTS_ASD_E=%EJSON_TESTS_ASD:\=\\%

@set TEST_EXP=^
(handler-case (sb-ext:exit :code (apply #'ejson-tests:main sb-ext:*posix-argv*))^
  (error ()^
    (sb-ext:exit :code 2 :abort t))^
  (sb-sys:interactive-interrupt ()^
    (sb-ext:exit :code -1073741510 :abort t)))

@sbcl^
 --noinform^
 --end-runtime-options^
 --no-sysinit^
 --no-userinit^
 --disable-debugger^
 --eval "(load """"~/quicklisp/setup.lisp"""")"^
 --eval "(asdf:load-asd #p""%EJSON_ASD_E%\"")"^
 --eval "(asdf:load-asd #p""%EJSON_TESTS_ASD_E%\"")"^
 --eval "(ql:quickload '#:ejson-tests)"^
 --eval "%TEST_EXP%"
@if %errorlevel% neq 0 exit /b %errorlevel%

@endlocal
