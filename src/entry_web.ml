module Entry = Rumen.Entry

let to_jv (entry : Entry.t) =
  let o = Jv.obj [||] in
  Jv.set o "url" (Jv.of_string entry.url) ;
  Jv.set o "title" (Jv.of_string entry.title) ;
  Jv.set o "tags"
    (Jv.of_jv_list (List.map (fun t -> Jv.of_string t) entry.tags)) ;
  Jv.set o "description" (Jv.of_string entry.description) ;
  Jv.set o "added" (Jv.of_float entry.added) ;
  o

let of_jv (jv : Jv.t) =
  let url = Jv.get jv "url" |> Jv.to_string in
  let title = Jv.get jv "title" |> Jv.to_string in
  let tags =
    Jv.get jv "tags" |> Jv.to_jv_list |> List.map (fun t -> Jv.to_string t)
  in
  let description = Jv.get jv "description" |> Jv.to_string in
  let added = Jv.get jv "added" |> Jv.to_float in
  {Entry.url; title; tags; description; content= None; added}

let filename (entry : Entry.t) =
  let format_date (ms : float) =
    let ms = Jv.of_float ms in
    let d = Jv.new' (Jv.get Jv.global "Date") [|ms|] in
    let year = Jv.to_int (Jv.call d "getFullYear" [||]) in
    let month = Jv.to_int (Jv.call d "getMonth" [||]) + 1 in
    let day = Jv.to_int (Jv.call d "getDate" [||]) in
    Printf.sprintf "%04d-%02d-%02d" year month day
  in
  let hash = Hashtbl.hash entry.url |> Printf.sprintf "%x" in
  let date = format_date entry.added in
  Printf.sprintf "articles/%s-%s.json" date hash
