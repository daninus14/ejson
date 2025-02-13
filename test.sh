base_dir=`pwd`
src_dir="$base_dir/src"
test_dir="$base_dir/test"
jzon_asd="$src_dir/ejson.asd"
jzon_test_asd="$test_dir/ejson-tests.asd"

test_exp="
(handler-case (sb-ext:exit :code (apply #'ejson-tests:main sb-ext:*posix-argv*))
  (error ()
    (sb-ext:exit :code 2 :abort t))
  (sb-sys:interactive-interrupt ()
    (sb-ext:exit :code -1073741510 :abort t)))
"

sbcl --noinform \
     --end-runtime-options \
     --no-sysinit \
     --no-userinit \
     --disable-debugger \
     --eval "(load #p\"~/quicklisp/setup.lisp\")" \
     --eval "(asdf:load-asd #p\"$jzon_asd\")" \
     --eval "(asdf:load-asd #p\"$jzon_test_asd\")" \
     --eval "(ql:quickload :ejson-tests)" \
     --eval "$test_exp"
    
