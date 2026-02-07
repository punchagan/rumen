open Brr
module Fetch = Brr_io.Fetch
module Storage = Brr_io.Storage
module W = Workflow
module Entry = Rumen.Entry

type config = {token: string; repo: string}

let committer_name = "Rumen App"

let committer_email = "rumen.app@example.com"

let config_to_jv config =
  Jv.obj
    [|("token", Jv.of_string config.token); ("repo", Jv.of_string config.repo)|]

let get_github_config () =
  let storage = Storage.local G.window in
  match Storage.get_item storage (Jstr.v "github_config") with
  | Some v -> (
    match Json.decode v with
    | Ok jv ->
        let token = Jv.get jv "token" in
        let repo = Jv.get jv "repo" in
        if Jv.is_none token || Jv.is_none repo then None
        else Some {token= Jv.to_string token; repo= Jv.to_string repo}
    | Error _ ->
        None )
  | None ->
      None

let update_path ~headers ~sha ~commit_message ~old url content =
  match Base64.data_utf_8_of_jstr content |> Base64.encode with
  | Error _ as e ->
      Fut.return e
  | Ok b64_content ->
      if
        Option.map
          (fun x ->
            let x' =
              x |> Jstr.to_string |> String.split_on_char '\n'
              |> String.concat "" |> Jstr.v |> Jstr.trim
            in
            Jstr.equal (Jstr.trim b64_content) x' )
          old
        |> Option.value ~default:false
      then (
        Console.log
          [ Jv.of_string
              (Printf.sprintf "No changes for %s, skipping update" url) ] ;
        let resp = Fetch.Response.v () in
        Fut.ok resp )
      else
        let committer =
          Jv.obj
            [| ("name", Jv.of_string committer_name)
             ; ("email", Jv.of_string committer_email) |]
        in
        let body =
          Jv.obj
            [| ("message", Jv.of_string commit_message)
             ; ("content", b64_content |> Jv.of_jstr)
             ; ("committer", committer) |]
        in
        ( match sha with
        | None ->
            ()
        | Some sha ->
            Jv.set body "sha" (Jv.of_string sha) ) ;
        let body_str = Json.encode body in
        let init =
          Fetch.Request.init ~headers ~method':(Jstr.v "PUT")
            ~body:(Fetch.Body.of_jstr body_str)
            ()
        in
        let req = Fetch.Request.v ~init (Jstr.v url) in
        Fetch.request req

let create_or_update_file ~path ~config ~commit_message content =
  let url =
    Printf.sprintf "https://api.github.com/repos/%s/contents/%s" config.repo
      path
  in
  let headers =
    Fetch.Headers.of_assoc
      [ (Jstr.v "Accept", Jstr.v "application/vnd.github.v3+json")
      ; (Jstr.v "Authorization", Jstr.v ("Bearer " ^ config.token))
      ; (Jstr.v "Content-Type", Jstr.v "application/json") ]
  in
  let init = Fetch.Request.init ~headers ~method':(Jstr.v "GET") () in
  let req = Fetch.Request.v ~init (Jstr.v url) in
  let open Fut.Result_syntax in
  let* resp = Fetch.request req in
  if Fetch.Response.status resp = 200 then
    let body = Fetch.Response.as_body resp in
    let open Fut.Result_syntax in
    let* text = Fetch.Body.text body in
    match Json.decode text with
    | Ok jv ->
        let old_content_b64 = Jv.get jv "content" |> Jv.to_jstr in
        let sha = Jv.get jv "sha" |> Jv.to_string in
        let commit_message = Printf.sprintf "Update: %s" commit_message in
        update_path ~headers ~sha:(Some sha) ~commit_message
          ~old:(Some old_content_b64) url content
    | Error e ->
        Fut.error e
  else
    let commit_message = Printf.sprintf "Add: %s" commit_message in
    update_path ~headers ~sha:None ~old:None ~commit_message url content

let save_entry_to_github entry =
  match get_github_config () with
  | None ->
      "GitHub configuration not found" |> Jstr.v |> Jv.Error.v |> Fut.error
  | Some config ->
      let json = Jv.get Jv.global "JSON" in
      (* Re-declare encode with spaces for pretty JSON files *)
      let encode v =
        Jv.to_jstr (Jv.call json "stringify" [|v; Jv.undefined; Jv.of_int 2|])
      in
      let content =
        Jstr.append (Entry_web.to_jv entry |> encode) (Jstr.v "\n")
      in
      let path = Entry_web.filename entry in
      let commit_message = entry.title in
      create_or_update_file ~commit_message ~config ~path content

let save_entries ~on_success ~on_done entries =
  let queue = entries |> List.to_seq |> Queue.of_seq in
  let ok = ref 0 in
  let rec process_next () =
    match Queue.take_opt queue with
    | None ->
        on_done !ok |> ignore ;
        Console.log [Jv.of_string "All entries have been processed."]
    | Some entry ->
        Console.log
          [Jv.of_string (Printf.sprintf "Syncing entry: %s" entry.Entry.url)] ;
        Fut.await (save_entry_to_github entry) (fun res ->
            ( match res with
            | Ok response when Fetch.Response.ok response ->
                on_success entry |> ignore ;
                ok := !ok + 1 ;
                Console.log
                  [ Jv.of_string
                      (Printf.sprintf "Successfully synced: %s" entry.Entry.url)
                  ]
            | Ok response ->
                Console.log
                  [ Jv.of_string
                      (Printf.sprintf "GitHub API error for %s: %d"
                         entry.Entry.url
                         (Fetch.Response.status response) ) ]
            | Error e ->
                Console.log
                  [ Jv.of_string
                      (Printf.sprintf "Failed to sync %s: %s" entry.Entry.url
                         (e |> Jv.Error.message |> Jstr.to_string) ) ] ) ;
            process_next () )
  in
  process_next ()

let update_workflow_file () =
  match get_github_config () with
  | None ->
      "GitHub configuration not found" |> Jstr.v |> Jv.Error.v |> Fut.error
  | Some config ->
      (* Update the GHA workflow file *)
      let content = W.workflow_yml |> Jstr.v in
      let path = ".github/workflows/generate.yml" in
      let commit_message = "generate workflow file" in
      let res_yml =
        create_or_update_file ~commit_message ~config ~path content
      in
      (* Update the bash script used by the workflow file *)
      let content = W.fetch_sh |> Jstr.v in
      let path = "fetch.sh" in
      let commit_message = "fetch script" in
      let res_sh =
        create_or_update_file ~commit_message ~config ~path content
      in
      let open Fut.Result_syntax in
      let* r1 = res_yml in
      let* r2 = res_sh in
      Fut.ok (r1, r2)
