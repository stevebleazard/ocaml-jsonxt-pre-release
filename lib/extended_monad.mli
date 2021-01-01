module type IO = Io.IO

module Make (IO : IO) : sig
  include (Reader_monad.Reader_monad with type json := Extended.Compliance.json and module IO := IO)
  include (Writer_monad_intf.Intf with type json := Extended.Compliance.json and module IO := IO)
end
