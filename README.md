# jsonxt - an OCaml streaming encoder and decoder

*jsonxt* implements a JSON streaming encoder and decoder with the following features:

* Support for various standard and extended JSON tree types:
  * Strict follows is a strict interpretation of RFC7158 and is compatible
    with jsonm
  * Basic extendeds the strict type to include convience types while maintaining
    RFC compliance.  This is compatible with yojson's Basic type
  * Extended adds additional non-standard types including tuples and variants
    and is not RFC compliant. This is compatible with yojson's Safe type
  * Stream is the raw JSON stream type before any conversions
* Support for string and channel I/O as well as user defined I/O via a simple functor
* Standard interfaces where appropriate including type `t` and `of_string` / `to_string`
  for strings
* RFC 7158 and 8259 compliant unless specified.
* RFC7464 JSON text sequences support
* Designed to be fast
