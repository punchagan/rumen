module Entry = Rumen.Entry
open Cmdliner
open Syndic

let generate_feed_from_entries ~feed_id ~content_dir entries =
  Printf.sprintf "Generating feed from %d entries ...\n" (List.length entries)
  |> print_endline ;
  let updated = Ptime.of_float_s (Unix.gettimeofday ()) |> Option.get in
  let feed_entries =
    List.map
      (fun (entry : Entry.t) ->
        let entry_id = Uri.of_string entry.url in
        let entry_title : Atom.text_construct = Text entry.title in
        let summary : Atom.text_construct = Text entry.description in
        let content =
          match entry.content with
          | Some hash ->
              (* Open the file content_dir/hash and return content  *)
              let filepath = Filename.concat content_dir hash in
              let content =
                if Sys.file_exists filepath then (
                  let ic = open_in filepath in
                  let len = in_channel_length ic in
                  let content = really_input_string ic len in
                  close_in ic ; content )
                else
                  "Article content is not available. (Content file not found)"
              in
              content
          | None ->
              "Article content is not available. (Content hash not set)"
        in
        let content : Atom.content = Html (Some entry_id, content) in
        let entry_updated =
          Ptime.of_float_s (entry.added /. 1000.)
          |> Option.value ~default:updated
        in
        let links = [Atom.link entry_id] in
        let authors = (Atom.author "Rumen feed generator", []) in
        Atom.entry ~summary ~content ~authors ~links ~id:entry_id
          ~title:entry_title ~updated:entry_updated () )
      entries
  in
  let id = Uri.of_string feed_id in
  let title : Atom.text_construct = Text "My Rumen Feed" in
  let feed = Atom.feed ~id ~title ~updated feed_entries in
  let oc = open_out "atom.xml" in
  Atom.output feed (`Channel oc)

let generate_feed ~feed_id ~n dir () =
  let cutoff_date =
    let now = Unix.time () in
    let n_days_ago = now -. float_of_int (n * 24 * 60 * 60) in
    Unix.gmtime n_days_ago
    |> fun tm ->
    Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
      tm.tm_mday
  in
  let content_dir = Filename.concat dir "content" in
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
  generate_feed_from_entries ~feed_id ~content_dir entries

(* fetch :: look for articles in the last n days which don't have content_hash
   value set and fetch content for them using the readability-cli. *)
let fetch_articles _dir _days =
  (* TODO: Fetch the content for articles by spwaning sub-processes running
     readable CLI commands *)
  ()

let articles_dir =
  let doc = "Directory to save articles in" in
  Arg.(value & opt string "./articles" & info ["d"; "dir"] ~doc)

and days =
  let doc = "Number of days to generate articles for" in
  Arg.(value & opt int 42 & info ["n"; "days"] ~doc)

and feed_id =
  let doc = "Feed ID URI" in
  Arg.(value & opt string "feed:example.com" & info ["id"] ~doc)

let generate_feed_cmd =
  let doc = "Generate feed with articles" in
  let term =
    let open Cmdliner.Term.Syntax in
    let+ articles_dir = articles_dir and+ days = days and+ feed_id = feed_id in
    generate_feed ~feed_id ~n:days articles_dir ()
  in
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
