let read_all_from_channel ic =
  let buf = Buffer.create 4096 in
  try
    while true do
      Buffer.add_channel buf ic 4096
    done ;
    assert false
  with End_of_file -> Buffer.contents buf

let fetch_content_for_url url =
  let cmd = Printf.sprintf "readable '%s' -p html-content 2>/dev/null" url in
  let ic = Unix.open_process_in cmd in
  let content = read_all_from_channel ic in
  let status = Unix.close_process_in ic in
  match status with
  | Unix.WEXITED 0 when String.length content > 0 ->
      Ok content
  | Unix.WEXITED code ->
      Error (Printf.sprintf "readable exited with code %d" code)
  | Unix.WSIGNALED code | Unix.WSTOPPED code ->
      Error (Printf.sprintf "readable killed by signal %d" code)

let parallel_iter ~max_concurrent f items =
  let running = ref 0 in
  let queue = Queue.of_seq (List.to_seq items) in
  let start_one () =
    match Queue.take_opt queue with
    | None ->
        ()
    | Some item -> (
      match Unix.fork () with 0 -> f item ; exit 0 | _pid -> incr running )
  in
  (* Start initial batch *)
  for _ = 1 to min max_concurrent (Queue.length queue) do
    start_one ()
  done ;
  (* As each finishes, start another *)
  while !running > 0 do
    let _ = Unix.wait () in
    decr running ; start_one ()
  done

let fetch_single_entry content_dir (filepath, entry) =
  Printf.printf "Fetching: %s\n%!" entry.Entry.url ;
  match fetch_content_for_url entry.url with
  | Ok content ->
      let hash = Digest.string content |> Digest.to_hex in
      let content_path = Filename.concat content_dir hash in
      let oc = open_out content_path in
      output_string oc content ;
      close_out oc ;
      let updated_entry = {entry with content= Some hash} in
      let json = Entry.to_yojson updated_entry in
      let oc = open_out filepath in
      Yojson.Safe.pretty_to_channel oc json ;
      output_char oc '\n' ;
      close_out oc ;
      Printf.printf "  -> %s: saved as %s\n%!" entry.url hash
  | Error err ->
      Printf.printf "  -> %s: failed: %s\n%!" entry.url err

let fetch_articles ~n ~max_concurrent dir =
  let cutoff_date =
    let now = Unix.time () in
    let n_days_ago = now -. float_of_int (n * 24 * 60 * 60) in
    Unix.gmtime n_days_ago
    |> fun tm ->
    Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
      tm.tm_mday
  in
  let content_dir = Filename.concat dir "content" in
  if not (Sys.file_exists content_dir) then Unix.mkdir content_dir 0o755 ;
  let entries_to_fetch =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun filename ->
        String.length filename >= 15
        && String.sub filename 0 10 >= cutoff_date
        && Filename.extension filename = ".json" )
    |> List.filter_map (fun filename ->
        let filepath = Filename.concat dir filename in
        let json = Yojson.Safe.from_file filepath in
        match Entry.of_yojson json with
        | Ok entry when Option.is_none entry.content ->
            Some (filepath, entry)
        | Ok _ ->
            None
        | Error err ->
            Printf.printf "Failed to parse %s: %s\n" filepath err ;
            None )
  in
  Printf.printf "Found %d entries to fetch content for\n%!"
    (List.length entries_to_fetch) ;
  parallel_iter ~max_concurrent
    (fetch_single_entry content_dir)
    entries_to_fetch
