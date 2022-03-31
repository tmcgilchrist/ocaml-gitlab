type binding = string * string  (* Key/value pair *)

type section = string * binding list (* Section name and contents *)

type t = section list
