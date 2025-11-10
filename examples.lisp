(ql:quickload "ejson")

CL-USER> (ejson:parse "{\"helloWorld\":192122,\"robertsmith\":\"ajwndj-awjd\", \"TitleWord\":2, \"CAPS\":2, \"camelCase\":2,  \"john\":\"awndMMMM\"}" :key-fn #'cl-change-case:param-case)

#<HASH-TABLE :TEST EQUAL :COUNT 6 {7005D6D863}>

#<HASH-TABLE {7005D6D863}>
--------------------
Count: 6
Size: 7
Test: EQUAL
Rehash size: 1.5
Rehash threshold: 1.0
[clear hashtable]
Contents: 
"camel-case" = 2 [remove entry]
"caps" = 2 [remove entry]
"hello-world" = 192122 [remove entry]
"john" = "awndMMMM" [remove entry]
"robertsmith" = "ajwndj-awjd" [remove entry]
"title-word" = 2 [remove entry]

CL-USER> (ejson:stringify *)
"{\"hello-world\":192122,\"robertsmith\":\"ajwndj-awjd\",\"title-word\":2,\"caps\":2,\"camel-case\":2,\"john\":\"awndMMMM\"}"

CL-USER> (setf ejson:*serialize-lisp-case-to-camel-case* t)
T

CL-USER> (ejson:parse "{\"helloWorld\":192122,\"robertsmith\":\"ajwndj-awjd\", \"TitleWord\":2, \"CAPS\":2, \"camelCase\":2,  \"john\":\"awndMMMM\"}" :key-fn #'cl-change-case:param-case)
#<HASH-TABLE :TEST EQUAL :COUNT 6 {7006BFABE3}>
CL-USER> (ejson:stringify *)
"{\"helloWorld\":192122,\"robertsmith\":\"ajwndj-awjd\",\"titleWord\":2,\"caps\":2,\"camelCase\":2,\"john\":\"awndMMMM\"}"
