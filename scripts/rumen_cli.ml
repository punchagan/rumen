open Rumen
open Cmdliner

let articles_dir =
  let doc = "Directory to save articles in" in
  Arg.(value & opt string "./articles" & info ["d"; "dir"] ~doc)

and days =
  let doc = "Number of days to generate articles for" in
  Arg.(value & opt int 42 & info ["n"; "days"] ~doc)

and feed_id =
  let doc = "Feed ID URI" in
  Arg.(value & opt string "feed:example.com" & info ["id"] ~doc)

and max_concurrent =
  let doc = "Maximum number of concurrent fetches" in
  Arg.(value & opt int 4 & info ["j"; "jobs"] ~doc)

let generate_feed_cmd =
  let doc = "Generate feed with articles" in
  let term =
    let open Cmdliner.Term.Syntax in
    let+ articles_dir = articles_dir and+ days = days and+ feed_id = feed_id in
    Generate.generate_feed ~feed_id ~n:days articles_dir ()
  in
  let info =
    Cmd.info "generate" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

and fetch_articles_cmd =
  let doc = "Fetch content for articles missing content" in
  let info = Cmd.info "fetch" ~doc in
  let term =
    let open Cmdliner.Term.Syntax in
    let+ dir = articles_dir
    and+ days = days
    and+ max_concurrent = max_concurrent in
    Fetch.fetch_articles ~max_concurrent ~n:days dir
  in
  Cmd.v info term

let default_cmd =
  let doc = "Rumen article fetcher and feed generator" in
  let term = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info =
    Cmd.info "rumen-cli" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.group ~default:term info [generate_feed_cmd; fetch_articles_cmd]

let () = exit (Cmd.eval default_cmd)
