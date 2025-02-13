## v1.1.4

Changes relative to [v1.1.3](#v113)

* Skip optional failing unit test on ECL (https://github.com/Zulu-Inuoe/ejson/issues/59)
* CLISP suport by removing PLN usage (https://github.com/Zulu-Inuoe/ejson/issues/53)
* Fix bogus float serialization for certain floats (thanks https://github.com/atgreen)

## v1.1.3

Changes relative to [v1.1.2](#v112)

* LispWorks 8 Structure Serialization
* Fix error when reading/writing from/to pathnames on LispWorks (https://github.com/Zulu-Inuoe/ejson/issues/56)
* Export `ejson:parser`
* Fix unit tests on LispWorks (https://github.com/Zulu-Inuoe/ejson/issues/57)
* Fix stack overflow on deeply nested JSON on SBCL 2.3.10+

## v1.1.2

Changes relative to [v1.1.1](#v111)

* Fix printing non-square multidimensional arrays https://github.com/Zulu-Inuoe/ejson/issues/43
* Fix serializing CLOS objects on LispWorks https://github.com/Zulu-Inuoe/ejson/issues/49
* `ejson:span` support for `cl:stream`
* Fix serializing non-square multidimensional arrays https://github.com/Zulu-Inuoe/ejson/pull/44
* Can now test ejson via `asdf:test-system` https://github.com/Zulu-Inuoe/ejson/pull/51

## v1.1.1

Changes relative to [v1.1.0](#v110)

* **Major fix** - Fix printing floating point without fractional or exponent parts. https://github.com/Zulu-Inuoe/ejson/issues/45

## v1.1.0

Changes relative to [v1.0.0](#v100)

* [ECL][ecl] Support https://github.com/Zulu-Inuoe/ejson/issues/36
* Add `ejson:parse-next-element` utility function for parsing a full element using the streaming reader.
* bugfix - signal `ejson:json-eof-error` when we encounter an incomplete unicode escape sequence such as `\uAD`. Used to signal `cl:type-error`.
* bugfix - `ejson:parse-next` no longer returns 3 values on `:object-key`
* Add `ejson:span` for easier subsequence support for `ejson:parse` and `ejson:parser` https://github.com/Zulu-Inuoe/ejson/issues/30
* `ejson:parse-next` no longer 'over-reads' strings, objects, and arrays.
* `allow-multiple-content` in reader functions now prevents ejson from scanning content after the toplevel object to signal error. Can be used to support formats like [JSON Lines][json-lines].
* faster parsing from (vector (unsigned-byte 8)) and binary streams https://github.com/Zulu-Inuoe/ejson/issues/29
* bugfix - `allow-trailing-comma` was not being properly applied when reading from vectors or pathnames
* bugfix - `max-string-length` was not being properly applied when reading from vectors or pathnames
* `max-depth` in reader/writer functions can now be set to `t` to indicate 'default'
* Fix Clozure Common Lisp, and LispWorks support https://github.com/Zulu-Inuoe/ejson/issues/27
* Add new `ejson:json-limit-error` conditions so users can discriminate between invalid JSON, and JSON that exceeds set limits
* Signal `ejson:json-write-error` rather than `error` when issuing invalid commands to `ejson:writer`
* Resolve issues around `max-string-length` and introduce the ability to choose default `t` and 'no limit' `nil`

Incompatible changes relative to [v1.0.0](#v100):

* `:key-fn` being `nil` now disables pooling, and `t` enables the default pool for `ejson:parse` and `ejson:make-parser`. This is for consistency with other arguments.

## v1.0.0

Initial Release

:tada:

[json-lines]: https://jsonlines.org/
[ecl]: https://gitlab.com/embeddable-common-lisp/ecl
