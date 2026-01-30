open Brr
module GH = Github

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
    let value = Entry.to_jv entry |> Json.encode in
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
  El.set_at (Jstr.v "class") (Some (Jstr.v ("status " ^ class_name))) el

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
    Window.set_location G.window new_uri

let show_config () = get_param "config" = "1"

let show_element id =
  let el = get_element id in
  El.set_class (Jstr.v "hidden") false el

let hide_element id =
  let el = get_element id in
  El.set_class (Jstr.v "hidden") true el

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
        let open Brr_io in
        match res with
        | Ok response ->
            if Fetch.Response.ok response then (
              LocalStorage.remove_entry entry |> ignore ;
              set_status "Saved entry to GitHub!" "success" )
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
  ; added }

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
      clear_param "config" ;
      show_element "entry-form" ;
      hide_element "setup-form"
  | Error e ->
      set_status
        (Printf.sprintf "Failed to save configuration: %s"
           (e |> Jv.Error.message |> Jstr.to_string) )
        "error"

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
  set_input_value "url" (get_param "url") ;
  set_input_value "title" (get_param "title") ;
  set_input_value "description" (get_param "text") ;
  (* Populate setup-form *)
  ( match GH.get_github_config () with
  | None ->
      ()
  | Some config ->
      set_input_value "github-token" config.token ;
      set_input_value "github-repo" config.repo ) ;
  let submit_ev = Ev.Type.create (Jstr.v "submit") in
  (* Attach entry-form handler *)
  let form = get_element "entry-form" in
  let _ = Ev.listen submit_ev handle_entry_submit (El.as_target form) in
  (* Attach setup-form handler *)
  let setup_form = get_element "setup-form" in
  let _ = Ev.listen submit_ev handle_config_submit (El.as_target setup_form) in
  Console.log [Jstr.v "rumen app initialized"]
