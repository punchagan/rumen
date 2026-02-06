module Console = Brr.Console
module Ev = Brr.Ev
module Fetch = Brr_io.Fetch
module Uri = Brr.Uri
module Cache = Fetch.Cache
module Cache_storage = Cache.Storage
module Sw = Brr_webworkers.Service_worker
module Brr_msg = Brr_io.Message

module Config = struct
  let c_shell = "shell-v1" |> Jstr.v

  let c_entries = "entries-v1" |> Jstr.v

  let caches = [c_shell; c_entries]

  let shell =
    [ "/"
    ; "/index.html"
    ; "/manifest.json"
    ; "/app.css"
    ; "/app.js"
    ; (* Icons *)
      "/icons/icon-192x192.png"
    ; "/icons/icon-512x512.png"
    ; "/icons/icon-maskable-192x192.png"
    ; "/icons/icon-maskable-512x512.png"
    ; (* Install screenshots *)
      "/screenshots/mobile.png"
    ; "/screenshots/desktop.png" ]
    |> List.map Jstr.v
end

module Fetch_strategy = struct
  let _network_request_and_cache ~cache request =
    let open Fut.Result_syntax in
    let* response = Fetch.request request in
    let clone = Fetch.Response.of_response response in
    let status = Fetch.Response.status response in
    (* Cache the request only on success *)
    if status >= 200 && status < 300 then
      Fetch.Cache.put cache request clone |> ignore ;
    Fut.ok response

  let cache_first_with_refresh request cache_name =
    let open Fut.Result_syntax in
    let storage = Fetch.caches () in
    let* cache = Cache_storage.open' storage cache_name in
    let* cached_response = Fetch.Cache.match' cache request in
    match cached_response with
    | Some response ->
        let _ = _network_request_and_cache ~cache request in
        Fut.ok response
    | None ->
        _network_request_and_cache ~cache request
end

let on_fetch e =
  let e = Ev.as_type e in
  let request = Fetch.Ev.request e in
  let url = Fetch.Request.url request in
  let _ =
    let open Result.Syntax in
    let* uri = Uri.of_jstr url in
    let path = Uri.path uri in
    ( match path with
    (* We update the SHELL on install, so use what's in the cache! But,
         refresh for next time. *)
    | s when List.mem s Config.shell ->
        let response =
          Fetch_strategy.cache_first_with_refresh request Config.c_shell
        in
        Fetch.Ev.respond_with e response
    | _ ->
        Console.log [Jv.of_string "SW fetch"; Jv.of_jstr path] ;
        () ) ;
    Result.ok ()
  in
  ()

let on_install e =
  let request_shell =
    let open Fut.Result_syntax in
    let storage = Fetch.caches () in
    let requests = List.map Fetch.Request.v Config.shell in
    let* cache = Cache.Storage.open' storage Config.c_shell in
    Fetch.Cache.add_all cache requests
  in
  Ev.Extendable.wait_until (Ev.as_type e) request_shell ;
  Sw.G.skip_waiting () |> ignore ;
  Console.(log [Jv.of_string "Service worker installed and resources cached."])

let on_activate e =
  let delete_old_caches =
    let open Fut.Result_syntax in
    let storage = Fetch.caches () in
    let* keys = Cache.Storage.keys storage in
    let old_caches =
      List.filter (fun k -> not (List.mem k Config.caches)) keys
    in
    let _ =
      List.iter (fun k -> Cache.Storage.delete storage k |> ignore) old_caches
    in
    Fut.ok ()
  in
  Sw.Clients.claim Sw.G.clients |> ignore ;
  Ev.Extendable.wait_until (Ev.as_type e) delete_old_caches ;
  Console.(log [Jv.of_string "Service worker activated."])

let () =
  let self = Ev.target_of_jv Jv.global in
  ignore (Ev.listen Fetch.Ev.fetch on_fetch self) ;
  ignore (Ev.listen Ev.install on_install self) ;
  ignore (Ev.listen Ev.activate on_activate self)
