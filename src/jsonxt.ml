module Json_basic = Json_basic

module Extended = struct
  include Json_extended
end

module Yojson = struct
  include Json_yojson
end

module Basic = struct
  include Json_basic
end

module Strict = struct
  include Json_strict
end

module Json = Json
