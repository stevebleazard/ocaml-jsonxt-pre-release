# TODO
## General
* Look at renaming or removing Yojson. If renamed will be Extended with Yojson style range restrictions
* Monad version
  * once rest of coding complete
  * Create compliant_lexxer_monad
  * Merge parser.ml into parser_monad
  * Create json_writer_monad
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
* json_writer.ml
  * Integrate into compliance levels
* json_writer.{ml,mli}
  * rename to json_to_string
* file writer
  * create json_writer_file
