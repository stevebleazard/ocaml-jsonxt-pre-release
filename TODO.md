# TODO
## General
* Look at renaming or removing Yojson. If renamed will be Extended with Yojson style range restrictions
* Monad version
  * once rest of coding complete
  * Create compliant_lexxer_monad
  * Merge parser.ml into parser_monad
  * Create suitable interface
## Code completion
* json_parse_types.ml
  * Review and clean up the compliance functions, remove un-needed ones
* lexxer_utils.ml
  * unroll add_char in single char case and check performance
* compliant_lexxer.mll
  * Tuples
  * Variants
* parser.ml
  * Tuples
  * Variants
* json_writer.ml
  * Integrate into compliance levels
