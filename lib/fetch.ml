let wayback_prefix = "https://web.archive.org/"

let read_all_from_channel ic =
  let buf = Buffer.create 4096 in
  try
    while true do
      Buffer.add_channel buf ic 4096
    done ;
    assert false
  with End_of_file -> Buffer.contents buf

let sanitize_wayback_html html =
  (* Replace any web.archive.org prefixed URLs with plain URLs. *)
  (* NOTE: The replace is a very dumb one, as of now, but should be good enough
     for most use cases. If we are trying to fetch the HTML for an archived
     article, for instance, this would break. Or any page containing examples
     of wayback machine URLs. But, we don't care about it, right now.*)
  let wayback_url_prefix_regex =
    Str.regexp {|https://web.archive.org/web/[0-9A-Za-z_-]+\/\(http\)|}
  in
  Str.global_replace wayback_url_prefix_regex {|\1|} html

let rec fetch_content_for_url ?(use_wayback = true) url =
  let user_agent =
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) \
     Chrome/89.0.4389.90 Safari/537.36"
  in
  let url =
    if use_wayback && not (String.starts_with ~prefix:wayback_prefix url) then
      wayback_prefix ^ url
    else url
  in
  let cmd =
    Printf.sprintf "readable -A '%s' -p html-content '%s' 2>/dev/null"
      user_agent url
  in
  let ic = Unix.open_process_in cmd in
  let content = read_all_from_channel ic in
  let status = Unix.close_process_in ic in
  match status with
  | Unix.WEXITED 0 when String.length content > 0 ->
      Ok (sanitize_wayback_html content)
  | Unix.WEXITED code ->
      if use_wayback then
        (* If fetching from Wayback failed, try fetching the original URL directly *)
        fetch_content_for_url ~use_wayback:false url
      else Error (Printf.sprintf "readable exited with code %d" code)
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
      Printf.printf "  -> %s : saved as %s\n%!" entry.url hash
  | Error err ->
      Printf.printf "  -> %s : failed: %s\n%!" entry.url err

let fetch_articles ~n ~max_concurrent dir =
  let content_dir = Filename.concat dir "content" in
  if not (Sys.file_exists content_dir) then Unix.mkdir content_dir 0o755 ;
  let entries_to_fetch = Entries.without_content ~n dir () in
  Printf.printf "Found %d entries to fetch content for\n%!"
    (List.length entries_to_fetch) ;
  parallel_iter ~max_concurrent
    (fetch_single_entry content_dir)
    entries_to_fetch
