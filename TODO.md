# TODO
## General
* Look at renaming or removing Yojson.
  * Extended.Yojson.xxx where xxx is the various levels
  * Compliance enforces yojson handling
    * integer (fail if exceeds int)
    * Names for NaN and Infinite
* Monad version
  * once rest of coding complete
  * Create compliant_lexxer_monad
  * Merge parser.ml into parser_monad
  * Create json_write_monad
  * Create suitable interface
