module type IO = Io.IO

module Make (IO : IO) = struct
  module Parser = Parser_monad.Make (Basic.Compliance) (IO)
  include Reader_monad.Make (Parser)
  type t = json
end
