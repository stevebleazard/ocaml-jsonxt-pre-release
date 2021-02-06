# TODO
## General
* Coding
  * Test input syntax errors - array, object, tuple, variant (extended only)
  * Async tests
  * Yojson
    * Review functionality
    * Test including replacing existing use of yojson library
    * Test derivers
    * Document
      * use in existing projects (create yojson.ml with include)
      * differences esp. missing functions
    * sort
    * pretty_print
    * utils
  * Stream.t json recovery (version 1.1?)
* Review
  * Compliance with RFCs
  * Test for compliance
    * https://github.com/nst/JSONTestSuite
  * Test extensions
    * Floatlit etc?
* Documentation
  * General - README.md with quick-start and examples including Deferred
    * performance
  * Function - review
  * Examples - add more
  * Yojson
    * replacing Yojson with Jsonxt - quick start
  * Async
* Testing
  * performance compared to Yojson
  * Yojson compatibility
* Release
  * dune release process
  * remove pkg dir?
