module Entry = Rumen.Entry
open Cmdliner
open Syndic

let generate_feed_from_entries entries =
  Printf.sprintf "Generating feed from %d entries ...\n" (List.length entries)
  |> print_endline ;
  let id = Uri.of_string "https://example.com/atom.xml" in
  let links = [Atom.link ~rel:Self id] in
  let title : Atom.text_construct = Text "My Rumen Feed" in
  let updated = Ptime.of_float_s (Unix.gettimeofday ()) |> Option.get in
  let feed_entries =
    List.map
      (fun (entry : Entry.t) ->
        let entry_id = Uri.of_string entry.url in
        let entry_title : Atom.text_construct = Text entry.title in
        let summary : Atom.text_construct = Text entry.description in
        let entry_updated =
          Ptime.of_float_s (entry.added /. 1000.)
          |> Option.value ~default:updated
        in
        let links = [Atom.link entry_id] in
        let authors = (Atom.author "Rumen feed generator", []) in
        Atom.entry ~summary ~authors ~links ~id:entry_id ~title:entry_title
          ~updated:entry_updated () )
      entries
  in
  let feed = Atom.feed ~links ~id ~title ~updated feed_entries in
  let oc = open_out "atom.xml" in
  Atom.output feed (`Channel oc)

let generate_feed dir n =
  let cutoff_date =
    let now = Unix.time () in
    let n_days_ago = now -. float_of_int (n * 24 * 60 * 60) in
    Unix.gmtime n_days_ago
    |> fun tm ->
    Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
      tm.tm_mday
  in
  let entries =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun filename ->
        (* Filename format: YYYY-MM-DD-<hash>.json *)
        String.length filename >= 15 && String.sub filename 0 10 >= cutoff_date )
    |> List.filter_map (fun filename ->
        let filepath = Filename.concat dir filename in
        let json = Yojson.Safe.from_file filepath in
        match Entry.of_yojson json with
        | Ok entry ->
            Some entry
        | Error err ->
            Printf.sprintf "Failed to parse entry from %s: %s" filepath err
            |> print_endline ;
            None )
    |> List.sort (fun a b -> Float.compare b.Entry.added a.Entry.added)
  in
  generate_feed_from_entries entries

(* fetch :: look for articles in the last n days which don't have content_hash
   value set and fetch content for them using the readability-cli. *)
let fetch_articles _dir _days =
  (* TODO *)
  ()

let articles_dir =
  let doc = "Directory to save articles in" in
  Arg.(value & opt string "./articles" & info ["d"; "dir"] ~doc)

and days =
  let doc = "Number of days to generate articles for" in
  Arg.(value & opt int 42 & info ["n"; "days"] ~doc)

let generate_feed_cmd =
  let doc = "Generate feed with articles" in
  let term = Term.(const generate_feed $ articles_dir $ days) in
  let info =
    Cmd.info "generate" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

and fetch_articles_cmd =
  let doc = "Fetch content for articles missing content_hash" in
  let info = Cmd.info "fetch" ~doc in
  let term = Term.(const fetch_articles $ articles_dir $ days) in
  Cmd.v info term

let default_cmd =
  let doc = "Rumen article fetcher and feed generator" in
  let term = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info =
    Cmd.info "rumen-cli" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.group ~default:term info [generate_feed_cmd; fetch_articles_cmd]

let () = exit (Cmd.eval default_cmd)
