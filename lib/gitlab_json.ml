(*
   Adapters used by atdgen to turn Github's representation of variants
   into an ATD-compatible representation.
*)
module Adapter = struct
  module WebhookEvent = Atdgen_runtime.Json_adapter.Type_field.Make (struct
    let type_field_name = "object_kind"
  end)
end
