open B0_kit.V000

(* OCaml library names *)

let gg = B0_ocaml.libname "gg"
let gg_kit = B0_ocaml.libname "gg.kit"

let vg = B0_ocaml.libname "vg"
let vg_htmlc = B0_ocaml.libname "vg.htmlc"

let evidence = B0_ocaml.libname "evidence"
let evidence_samples = B0_ocaml.libname "evidence.samples"

let brr = B0_ocaml.libname "brr"
let note = B0_ocaml.libname "note"
let note_brr = B0_ocaml.libname "note.brr"

let vz = B0_ocaml.libname "vz"
let vz_plot = B0_ocaml.libname "vz.plot"
let vz_doc = B0_ocaml.libname "vz.doc"
let vz_canvas = B0_ocaml.libname "vz.canvas"

(* Libraries *)

let evidence_lib =
  let srcs = [`File ~/"src/evidence.mli"; `File ~/"src/evidence.ml"] in
  B0_ocaml.lib evidence ~srcs

let evidence_samples_lib =
  let srcs = [`Dir ~/"samples"] in
  let requires = [evidence] in
  B0_ocaml.lib evidence_samples ~requires ~srcs

let vz_lib =
  let srcs = [ `File ~/"src/vz.mli"; `File ~/"src/vz.ml" ] in
  let requires = [gg; gg_kit; vg] in
  B0_ocaml.lib vz ~srcs ~requires

let vz_plot_lib =
  let srcs = [`File ~/"src/vz_plot.mli"; `File ~/"src/vz_plot.ml"] in
  let requires = [gg; gg_kit; vg; vg_htmlc; vz; evidence] in
  B0_ocaml.lib vz_plot ~srcs ~requires

let vz_canvas_lib =
  let srcs = [`File ~/"src/vz_canvas.mli"; `File ~/"src/vz_canvas.ml"] in
  let requires = [gg; gg_kit; vg; vz; brr] in
  B0_ocaml.lib vz_canvas ~srcs ~requires

let vz_doc_lib =
  let srcs = [`File ~/"src/vz_doc.mli"; `File ~/"src/vz_doc.ml"] in
  let requires = [gg; vg; vg_htmlc; brr; note; note_brr] in
  B0_ocaml.lib vz_doc ~srcs ~requires

(* Tests *)

let test_exe ?(requires = []) src ~doc =
  let srcs = [`File src] in
  let meta = B0_meta.empty |> B0_meta.(tag test) in
  let requires = gg :: gg_kit :: vg :: vz :: requires in
  B0_ocaml.exe (Fpath.basename ~no_ext:true src) ~meta ~requires ~srcs ~doc

let test = test_exe ~/"test/test.ml" ~doc:"Vz tests"

let test_with_brr ?(requires = []) src ~doc =
  let srcs = [`File src] in
  let requires = gg :: gg_kit :: vg :: vz :: vg_htmlc :: brr :: requires in
  let assets_root = Fpath.v "test" in
  let meta =
    B0_meta.empty
    |> B0_meta.add B0_jsoo.compilation_mode `Separate
    |> B0_meta.add B0_jsoo.source_map (Some `Inline)
  in
  let name = Fpath.basename ~no_ext:true src in
  B0_jsoo.web name ~srcs ~requires ~assets_root ~meta ~doc

let test_iris =
  let requires = [evidence; evidence_samples] in
  test_with_brr ~/"test/test_iris.ml" ~requires ~doc:"Test iris"

let test_dev =
  let requires = [evidence; vz_plot; evidence_samples] in
  test_with_brr ~/"test/test_dev.ml" ~requires ~doc:"Test dev"

let test_isolines =
  let requires = [vz_canvas] in
  test_with_brr ~/"test/isolines.ml" ~requires ~doc:"Test isolines"

let test_metaballs =
  let requires = [vz_canvas] in
  test_with_brr ~/"test/metaballs.ml" ~requires ~doc:"Test isolines"

let test_doc =
  let requires = [note; vz_doc] in
  test_with_brr ~/"test/test_doc.ml" ~requires ~doc:"Test doc"

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The vz programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/vz"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/vz/doc/"
    |> B0_meta.(add licenses) ["ISC"; "Apache-2.0"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/vz.git"
    |> B0_meta.(add issues) "https://github.com/dbuenzli/vz/issues"
    |> B0_meta.(add description_tags)
      ["graphics"; "visualization"; "declarative"; "graphics";
       "org:erratique"]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "gg", {|>= "1.0.0"|};
        "vg", {|>= "0.9.4"|};
        "brr", {|>= "0.0.6"|};
        "note", {||} ]
    |> B0_meta.add B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.make "default" ~meta ~locked:true ~doc:"vz package" @@
  B0_unit.list ()
