let token_to_string (tok:Tokens.token) =
  match tok with
  | STRING s -> s
  | OS -> "{"
  | OE -> "}"
  | NULL -> "Null"
  | NEGINFINITY -> "-Infinity"
  | NAN -> "NAN"
  | LEX_ERROR err -> "input error '" ^ err ^ "'"
  | LARGEINT s -> s
  | INT i -> Int.to_string i
  | INFINITY -> "Infinity"
  | FLOAT f -> Float.to_string f
  | EOF -> "end-of-file"
  | COMPLIANCE_ERROR err -> err
  | COMMA -> ","
  | COLON -> ":"
  | BOOL b -> if b then "true" else "false"
  | AS -> "["
  | AE -> "]"
  | VS -> "<"
  | VE -> ">"
  | TS -> "("
  | TE -> ")"
