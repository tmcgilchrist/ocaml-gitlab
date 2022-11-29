(*
   Adapters used by atdgen to turn GitLab's representation of variants
   into an ATD-compatible representation.
*)
module Adapter = struct
  module WebhookEvent = Atdgen_runtime.Json_adapter.Type_field.Make (struct
    let type_field_name = "object_kind"
  end)
end

exception Parse_error of Yojson.Safe.t * string

module Date = struct
  type t = float

  let wrap str =
    try ISO8601.Permissive.date str
    with Failure _ ->
      raise
        (Parse_error
           ( `String str,
             Format.sprintf "%s: Date.wrap can't parse date format YYYY-MM-DD"
               str ))

  let unwrap = ISO8601.Permissive.string_of_date
end

module DateTime = struct
  type t = float

  let wrap str =
    try ISO8601.Permissive.datetime str
    with Failure _ ->
      raise
        (Parse_error
           ( `String str,
             Format.sprintf "%s: DateTime.wrap can't parse ISO8601 date" str ))

  let unwrap date = ISO8601.Permissive.string_of_datetimezone (date, 0.)
end
