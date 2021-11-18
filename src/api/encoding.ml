open Json_encoding

let api_config = obj1 (opt "port" int)

type response = Data_types.response = {
  generated : bool;
} [@@deriving json_encoding]