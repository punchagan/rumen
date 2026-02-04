let get ~n dir () =
  let cutoff_date =
    let now = Unix.time () in
    let n_days_ago = now -. float_of_int (n * 24 * 60 * 60) in
    Unix.gmtime n_days_ago
    |> fun tm ->
    Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
      tm.tm_mday
  in
  let entries_with_filepath =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun filename ->
        (* Filename format: YYYY-MM-DD-<hash>.json *)
        String.length filename >= 15
        && String.sub filename 0 10 >= cutoff_date
        && Filename.extension filename = ".json" )
    |> List.filter_map (fun filename ->
        let filepath = Filename.concat dir filename in
        let json = Yojson.Safe.from_file filepath in
        match Entry.of_yojson json with
        | Ok entry ->
            Some (filepath, entry)
        | Error err ->
            Printf.sprintf "Failed to parse entry from %s: %s" filepath err
            |> print_endline ;
            None )
    |> List.sort (fun (_, a) (_, b) ->
        Float.compare b.Entry.added a.Entry.added )
  in
  entries_with_filepath

let with_content ~n dir () =
  let entries_with_filepath = get ~n dir () in
  List.filter
    (fun (_, entry) -> Option.is_some entry.Entry.content)
    entries_with_filepath

let without_content ~n dir () =
  let entries_with_filepath = get ~n dir () in
  List.filter
    (fun (_, entry) -> Option.is_none entry.Entry.content)
    entries_with_filepath
