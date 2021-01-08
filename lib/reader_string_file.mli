module type Reader_string_file = sig
  type json

  (** [Reader_string_file] supports functions to parse JSON data from various sources.
      The interface includes two type of parser
      - Single value parsers that return a single Json tree
      - [json Stream.t] functions that process a stream of JSON data, possibly seperated by whitespace.
        eg. [{"a": 1}  {"b":2}] would result in 2 Json values being returned by the stream
  *)

  (** [json_of_string string] converts [string] to a [json] value returing an error
      if the string has syntax, grammar or compliance errors *)
  val json_of_string : string -> (json, string) result

  (** [json_of_string_exn string] converts [string] to a [json] value raising a
      [Failure] exception if the string has syntax, grammar or compliance errors *)
  val json_of_string_exn : string -> json

  (** [json_of_file file] converts the text from [file] to a [json] value returing an error
      if the file contents have syntax, grammar or compliance errors. The file is closed on error *)
  val json_of_file : string -> (json, string) result

  (** [json_of_file file] converts the text from [file] to a [json] value raising
      a [Failure] exception if the file contents have syntax, grammar or compliance errors.
      The file is closed on error *)
  val json_of_file_exn : string -> json

  (** [json_of_channel channel] converts the text from [channel] to a [json] value
      returing an error if the channel contents have syntax, grammar or compliance errors.
      The channel is not closed *)
  val json_of_channel : in_channel -> (json, string) result

  (** [json_of_channel channel] converts the text from [channel] to a [json] value raising
      a [Failure] exception if the channel contents have syntax, grammar or compliance errors.
      The file is not closed *)
  val json_of_channel_exn : in_channel -> json

  (** [json_of_function f] converts text provided by [f] to a [json] value
      returing an error if the supplied text has syntax, grammar or
      compliance errors.  The function [f buf len] takes a [bytes] [buf] buffer, the
      maximum number of bytes to read [len] and returns the number of bytes read.
      Returning 0 indicates end-of-file *)
  val json_of_function : (bytes -> int -> int) -> (json, string) result

  (** [json_of_function_exn f] converts text provided by [f] to a [json] value
      raising a [Failure] exception if the channel contents have syntax, grammar or
      compliance errors.  See [json_of_function] for detail of function [f] *)
  val json_of_function_exn : (bytes -> int -> int) -> json

  (** [of_string] is an alias for json_of_string_exn *)
  val of_string : string -> json

  (** [of_file] is an alias for json_of_file_exn *)
  val of_file : string -> json

  (** [of_channel] is an alias for json_of_channel_exn *)
  val of_channel : in_channel -> json

  (** [of_function] is an alias for json_of_function_exn *)
  val of_function : (bytes -> int -> int) -> json

  (** {2 [Stream.t] readers}

      [Stream.t] readers provide a mechanism to read a stream of JSON values. eg
      {[
        {"datapoint": 1, "value": 2}
        {"datapoint": 2, "value": 5}
      ]}
      *)

  (** [stream_from_string string] converts [string] containing zero or more json
      object to a [json Stream.t] value raising a [Failure] exception if the
      string has syntax, grammar or compliance errors *)
  val stream_from_string : string -> json Stream.t

  (** [stream_from_channel in_channel] converts the text from [in_channel], containing
      zero or more json objects, to a [json Stream.t] value raising a [Failure] exception
      if the file has syntax, grammar or compliance errors *)
  val stream_from_channel : in_channel -> json Stream.t

  (** [stream_from_function f] converts text provided by [f], containing zero
      of more JSON objects, to a [json Stream.t] value raising a [Failure] exception
      if the file has syntax, grammar or compliance errors.  The function [f buf len]
      takes a [bytes] [buf] buffer, the maximum number of bytes to read [len] and
      returns the number of bytes read.  Returning 0 indicates end-of-file *)
  val stream_from_function : (bytes -> int -> int) -> json Stream.t
end

module Make (Lexxer : Compliant_lexxer.Lex ) (Parser : Parser.Parser) : Reader_string_file
  with type json = Parser.Compliance.json

