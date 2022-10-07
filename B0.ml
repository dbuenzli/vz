open B0_kit.V000

(* OCaml library names *)

let gg = B0_ocaml.libname "gg"
let gg_kit = B0_ocaml.libname "gg.kit"
let vg = B0_ocaml.libname "vg"

let evidence = B0_ocaml.libname "evidence"
let evidence_samples = B0_ocaml.libname "evidence.samples"

let vz = B0_ocaml.libname "vz"
let vz_plot = B0_ocaml.libname "vz.plot"

(* Libraries *)

let evidence_lib =
  let srcs = Fpath.[ `File (v "src/evidence.mli");
                     `File (v "src/evidence.ml") ] in
  let requires = [] in
  let doc = "The evidence library" in
  B0_ocaml.lib evidence ~doc ~srcs ~requires

let evidence_samples_lib =
  let srcs = Fpath.[`Dir (v "samples");] in
  let requires = [evidence] in
  let doc = "The evidence.samples library" in
  B0_ocaml.lib evidence_samples ~doc ~srcs ~requires

let vz_lib =
  let srcs = Fpath.[ `File (v "src/vz.mli"); `File (v "src/vz.ml") ] in
  let requires = [gg; gg_kit; vg] in
  B0_ocaml.lib vz ~doc:"The vz library" ~srcs ~requires

let vz_plot_lib =
  let srcs = Fpath.[`File (v "src/vz_plot.mli"); `File (v "src/vz_plot.ml")] in
  let requires = [gg; gg_kit; vg; vz; evidence] in
  B0_ocaml.lib vz_plot ~doc:"The vz.plot library" ~srcs ~requires

(* Tests *)

let test_exe ?(requires = []) src ~doc =
  let src = Fpath.v src in
  let srcs = Fpath.[`File src] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = gg :: gg_kit :: vg :: vz :: requires in
  B0_ocaml.exe (Fpath.basename ~no_ext:true src) ~srcs ~doc ~meta ~requires

let test = test_exe "test/test.ml" ~doc:"Vz tests"

let brr = B0_ocaml.libname "brr"
let vg_htmlc = B0_ocaml.libname "vg.htmlc"

let test_with_brr ?(requires = []) src ~doc =
  let src = Fpath.v src in
  let srcs = Fpath.[`File src; `File (v "test/style.css")] in
  let requires = gg :: gg_kit :: vg :: vz :: vg_htmlc :: brr :: requires in
  let assets_root = Fpath.v "test" in
  let meta =
    let comp_mode = `Separate in
    B0_jsoo.meta ~requires ~assets_root ~comp_mode ~source_map:(Some `Inline) ()
  in
  B0_jsoo.web (Fpath.basename ~no_ext:true src) ~doc ~srcs ~meta

let test_iris =
  let requires = [evidence; evidence_samples] in
  test_with_brr "test/test_iris.ml" ~requires ~doc:"Test iris"

let test_dev =
  let requires = [evidence; vz_plot; evidence_samples] in
  test_with_brr "test/test_dev.ml" ~requires ~doc:"Test dev"


(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> tag B0_opam.tag
    |> add authors ["The vz programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/vz"
    |> add online_doc "https://erratique.ch/software/vz/doc/"
    |> add licenses ["ISC"; "Apache-2.0"]
    |> add repo "git+https://erratique.ch/repos/vz.git"
    |> add issues "https://github.com/dbuenzli/vz/issues"
    |> add description_tags
      ["graphics"; "visualization"; "declarative"; "graphics";
       "org:erratique"]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "gg", {|>= "1.0.0"|};
        "vg", {|>= "0.9.4"|};
        "brr", {|with-test|};
      ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.v "default" ~doc:"vz package" ~meta ~locked:true @@
  B0_unit.list ()
