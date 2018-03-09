module Json_extended = Json_basic
module Json_yojson = Json_yojson
module Json_basic = Json_basic
module Json_strict = Json_strict
module Json = Json

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
