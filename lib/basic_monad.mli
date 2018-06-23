module type IO = Io.IO

module Make (IO : IO) : sig
  module Parser : Parser_monad.Parser with 
        module IO = IO
    and module Compliance = Basic.Compliance
  include (Reader_monad.Reader_monad with module IO := IO)
end
