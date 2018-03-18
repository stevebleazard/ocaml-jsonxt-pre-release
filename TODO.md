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
## Code completion
* json_parse_types.ml
  * Review and clean up the compliance functions, remove un-needed ones
* compliant_lexxer.mll
  * Tuples
  * Variants
* parser.ml
  * Tuples
  * Variants
* writer
  * rename json_writer.ml json_write_string.ml
  * Integrate into compliance levels
    * move to using a functor using the Compliance module
    * strict - no nan/inf/-inf
  * createson_write_file.ml
