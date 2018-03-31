# TODO
## General
* Coding
  * Stream encode / decode
  * Streaming json with recovery
* Review
  * Compliance with RFCs
  * Test for compliance (https://github.com/nst/JSONTestSuite)
    * Create a test suite runner: test_compliance [succeed|fail|either] [file...]
  * Test extensions
    * Integer handling in each compliance level
    * Tuples and Variants
    * Floatlit etc?
* Documentation
  * General
  * Function
  * Examples
* Monad version
  * once rest of coding complete
  * Create compliant_lexxer_monad
  * Merge parser.ml into parser_monad
  * Create json_write_monad
  * Create suitable interface
