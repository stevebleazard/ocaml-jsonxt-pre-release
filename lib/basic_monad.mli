module type IO = Io.IO

module Make (IO : IO) : sig
  include (Reader_monad.Reader_monad with module IO := IO)
  include (Writer_monad_intf.Intf with type json := Basic.Compliance.json and module IO := IO)
end
