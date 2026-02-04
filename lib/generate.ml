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
  let content_dir = Filename.concat dir "content" in
  let entries = Entries.with_content ~n dir () |> List.map snd in
  generate_feed_from_entries ~feed_id ~content_dir entries
