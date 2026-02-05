(lang dune 3.0)

(authors "Puneeth Chaganti <punchagan@muse-amuse.in>")

(maintainers "Puneeth Chaganti <punchagan@muse-amuse.in>")

(license AGPLv3)

(documentation https://github.com/punchagan/rumen)

(source
 (github punchagan/rumen))

(package
 (name rumen)
 (synopsis
  "A PWA to save URLs to a GitHub repo and generate full-content RSS feeds")
 (description
  "A PWA with a share target that saves URLs to a GitHub repo, with a GitHub action to generate RSS feeds with full article content.")
 (allow_empty)
 (depends
  ocaml
  brr
  cmdliner
  ppx_deriving_yojson
  yojson
  syndic
  (alcotest :with-test))
 (tags
  ("rss" "feeds" "PWA" "offline" "read-later")))
