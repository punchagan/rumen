open Brr
open Brr_io
module GH = Github
module Entry = Rumen.Entry
module Entry_w = Entry_web

module LocalStorage = struct
  module Storage = Brr_io.Storage

  let config_key = Jstr.v "github_config"

  let storage = Storage.local G.window

  let has_key key =
    match Storage.get_item storage key with Some _ -> true | None -> false

  let has_config () = has_key config_key

  let save_config config =
    let data = GH.config_to_jv config |> Json.encode in
    Storage.set_item storage config_key data

  let add_entry (entry : Entry.t) =
    let key = Jstr.v ("entry:" ^ entry.url) in
    let value = Entry_w.to_jv entry |> Json.encode in
    Storage.set_item storage key value

  let remove_entry (entry : Entry.t) =
    let key = Jstr.v ("entry:" ^ entry.url) in
    Storage.remove_item storage key
end

let get_element id =
  match Document.find_el_by_id G.document (Jstr.v id) with
  | Some el ->
      el
  | None ->
      failwith (Printf.sprintf "Element not found: %s" id)

let get_input_value id =
  let el = get_element id in
  Jv.get (El.to_jv el) "value" |> Jv.to_string

let set_input_value id value =
  let el = get_element id in
  Jv.set (El.to_jv el) "value" (Jv.of_string value)

let set_status msg class_name =
  let el = get_element "status" in
  El.set_children el [El.txt (Jstr.v msg)] ;
  El.set_at At.Name.class' (Some (Jstr.v ("status " ^ class_name))) el

let set_status_with_link ~url msg class_name =
  let el = get_element "status" in
  let link = El.a ~at:[At.href (Jstr.v url)] [El.txt (Jstr.v msg)] in
  El.set_children el [link] ;
  El.set_at At.Name.class' (Some (Jstr.v ("status " ^ class_name))) el

let get_param key =
  let params = Window.location G.window |> Uri.query_params in
  Uri.Params.find (Jstr.v key) params
  |> function Some result -> Jstr.to_string result | None -> ""

let clear_param key =
  let uri = Window.location G.window in
  let params = Uri.query_params uri in
  if not (Uri.Params.mem (Jstr.v key) params) then ()
  else
    let params =
      Uri.Params.to_assoc params
      |> List.filter (fun (k, _) -> not (Jstr.equal k (Jstr.v key)))
      |> Uri.Params.of_assoc
    in
    let new_uri = Uri.with_query_params uri params in
    Window.History.replace_state ?uri:(Some new_uri) (Window.history G.window)

let split_url_from_text text =
  let url_prefix_regex = Str.regexp "http[s]?://" in
  match Str.search_backward url_prefix_regex text (String.length text - 1) with
  | exception Not_found ->
      (text, "")
  | pos ->
      let url = String.sub text pos (String.length text - pos) in
      let text = String.sub text 0 pos in
      (text, url)

let show_config () = get_param "config" = "1"

let show_element id =
  let el = get_element id in
  El.set_class (Jstr.v "hidden") false el

let hide_element id =
  let el = get_element id in
  El.set_class (Jstr.v "hidden") true el

let set_bookmarklet_url url =
  match Uri.of_jstr url with
  | Error _ ->
      hide_element "bookmarklet-container"
  | Ok uri ->
      let host = Uri.host uri |> Jstr.to_string in
      let url = Jstr.to_string url in
      let bookmarklet_code =
        Printf.sprintf
          "javascript:(function(){window.open('%s?url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)+'&text='+encodeURIComponent(window.getSelection().toString())+(location.hostname==='%s'?'&config=1':''))})()"
          url host
      in
      let el = get_element "bookmarklet" in
      El.set_at At.Name.href (Some (Jstr.v bookmarklet_code)) el ;
      show_element "bookmarklet-container"

let save_entry entry =
  (* TODO: Maybe save to IndexedDB so that service worker can background
     sync, if required. *)
  let saved =
    match LocalStorage.add_entry entry with
    | Ok () ->
        true
    | Error e ->
        Console.log
          [ Jstr.v
              (Printf.sprintf "Storage error for %s: %s" entry.url
                 (e |> Jv.Error.message |> Jstr.to_string) ) ] ;
        false
  in
  if Navigator.online G.navigator then
    Fut.await (GH.save_entry_to_github entry) (fun res ->
        match res with
        | Ok response ->
            if Fetch.Response.ok response then (
              LocalStorage.remove_entry entry |> ignore ;
              let open Fut.Result_syntax in
              let _ =
                let* data =
                  Fetch.Response.as_body response |> Fetch.Body.json
                in
                let url =
                  Jv.get (Jv.get data "content") "html_url" |> Jv.to_string
                in
                set_status_with_link ~url "Saved entry to GitHub!" "success" ;
                Fut.ok ()
              in
              () )
            else
              set_status
                (Printf.sprintf "GitHub API error: %d"
                   (Fetch.Response.status response) )
                "error"
        | Error e ->
            Console.log
              [ Jstr.v
                  (Printf.sprintf "Failed to save entry to GitHub: %s"
                     (e |> Jv.Error.message |> Jstr.to_string) ) ] ) ;
  saved

let entry_of_form () =
  let tags_str = get_input_value "tags" in
  let tags =
    String.split_on_char ',' tags_str
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  let added = Jv.call (Jv.get Jv.global "Date") "now" [||] |> Jv.to_float in
  { Entry.url= get_input_value "url"
  ; title= get_input_value "title"
  ; tags
  ; description= get_input_value "description"
  ; content= None
  ; added }

let entry_to_form entry =
  set_input_value "url" entry.Entry.url ;
  set_input_value "title" entry.title ;
  set_input_value "description" entry.description ;
  set_input_value "tags" (String.concat ", " entry.tags)

let entry_of_params () =
  let url = get_param "url" in
  let title = get_param "title" in
  let description = get_param "text" in
  (* On Android, the ACTION_SEND intent used to share data from one activity to
     another only has EXTRA_TITLE and EXTRA_TEXT. So, the `url` parameter can
     be empty and the URL ends up being in the `text`. *)
  let description, url =
    match (description, url) with
    | "", "" ->
        ("", "")
    | description, "" ->
        if String.starts_with ~prefix:"http" description then (description, "")
        else
          (* When a PWA shares with data containing of the type {url; title; text},
           we seem to receive text parameter with the URL appended to the text
           content. *)
          split_url_from_text description
    | description, url ->
        (description, url)
  in
  {Entry.url; title; tags= []; description; content= None; added= 0.}

let clear_form () =
  List.iter
    (fun id -> set_input_value id "")
    ["url"; "title"; "tags"; "description"]

let handle_config_submit ev =
  Ev.prevent_default ev ;
  Ev.stop_propagation ev ;
  let token = get_input_value "github-token" in
  let repo = get_input_value "github-repo" in
  let config = {GH.token; repo} in
  match LocalStorage.save_config config with
  | Ok () ->
      set_status "Configuration saved!" "success" ;
      show_element "entry-form" ;
      hide_element "setup-form"
  | Error e ->
      set_status
        (Printf.sprintf "Failed to save configuration: %s"
           (e |> Jv.Error.message |> Jstr.to_string) )
        "error"

let handle_save_config_and_update_workflow ev =
  Ev.prevent_default ev ;
  Ev.stop_propagation ev ;
  handle_config_submit ev ;
  Console.log [Jv.of_string "Updating GitHub workflow..."] ;
  Fut.await (GH.update_workflow_file ()) (fun res ->
      clear_param "config" ;
      match res with
      | Ok (r1, r2) when Fetch.Response.ok r1 && Fetch.Response.ok r2 ->
          set_status "GitHub workflow updated!" "success"
      | Ok (r1, r2) ->
          let error_status =
            max (Fetch.Response.status r1) (Fetch.Response.status r2)
          in
          set_status
            (Printf.sprintf "GitHub API error: %d" error_status)
            "error"
      | Error e ->
          set_status
            (Printf.sprintf "Failed to update workflow: %s"
               (e |> Jv.Error.message |> Jstr.to_string) )
            "error" )

let handle_entry_submit ev =
  Ev.prevent_default ev ;
  Ev.stop_propagation ev ;
  let entry = entry_of_form () in
  if save_entry entry then (
    set_status "Saved entry!" "success" ;
    clear_form () )
  else set_status "Failed to save entry." "error"

let () =
  if LocalStorage.has_config () && not (show_config ()) then (
    show_element "entry-form" ; hide_element "setup-form" )
  else (show_element "setup-form" ; hide_element "entry-form") ;
  (* Populate entry-form *)
  entry_of_params () |> entry_to_form ;
  (* Populate setup-form *)
  ( match GH.get_github_config () with
  | None ->
      ()
  | Some config ->
      set_input_value "github-token" config.token ;
      set_input_value "github-repo" config.repo ) ;
  let submit_ev = Ev.Type.create (Jstr.v "submit") in
  (* Set bookmarklet url *)
  let uri = Window.location G.window in
  let origin = Jv.get (Uri.to_jv uri) "origin" |> Jv.to_jstr in
  set_bookmarklet_url origin ;
  (* Attach entry-form handler *)
  let form = get_element "entry-form" in
  let _ = Ev.listen submit_ev handle_entry_submit (El.as_target form) in
  (* Attach setup-form handler *)
  let setup_form = get_element "setup-form" in
  let _ =
    Ev.listen submit_ev handle_save_config_and_update_workflow
      (El.as_target setup_form)
  in
  Console.log [Jstr.v "rumen app initialized"]
