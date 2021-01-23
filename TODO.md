# TODO
## General
* Coding
  * Test input syntax errors - array, object, tuple, variant (extended only)
  * Yojson
    * Review functionality
    * Test including replacing existing use of yojson library
    * Test derivers
    * lexer_state - accept but ignore
    * Document
      * use in existing projects (create yojson.ml with include)
      * differences esp. missing functions
  * Stream.t json recovery (version 1.1?)
  * lexbuf readers
  * error_info return
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
* Testing
  * performance compared to Yojson
  * Yojson compatibility
