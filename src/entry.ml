type t =
  { url: string
  ; title: string
  ; tags: string list
  ; description: string
  ; added: float }
[@@deriving yojson]
