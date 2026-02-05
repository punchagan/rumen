module F = Rumen.Fetch

let ( / ) = Filename.concat

let sanitize_wayback_html_tests =
  let test_data name =
    let ic = open_in ("data" / name) in
    let input = F.read_all_from_channel ic in
    close_in ic ; input
  in
  let web_archive_regex = Str.regexp "web\\.archive\\.org" in
  List.map
    (fun fname ->
      let input = test_data fname in
      let test_name = Printf.sprintf "sanitize_wayback_html from %s" fname in
      Alcotest.test_case test_name `Quick (fun () ->
          let sanitized = F.sanitize_wayback_html input in
          print_endline sanitized ;
          Alcotest.check_raises "No web.archive.org URLs" Not_found (fun () ->
              Str.search_forward web_archive_regex sanitized 0 |> ignore ) ) )
    ["1.html"]

let () =
  Alcotest.run "Rumen Lib Tests"
    [("Sanitize Wayback HTML", sanitize_wayback_html_tests)]
