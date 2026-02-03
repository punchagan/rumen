type t =
  { url: string
  ; title: string
  ; tags: string list
  ; description: string
  ; content: string option [@default None]
  ; added: float }
[@@deriving yojson {strict= false}]
