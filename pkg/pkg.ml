#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "vz" @@ fun c ->
  Ok [
    Pkg.mllib "src/vz.mllib";
    Pkg.mllib "src/vz_plot.mllib" ~dst_dir:"plot";
    Pkg.mllib "src/vz_doc.mllib" ~dst_dir:"doc";
    Pkg.mllib "src/evidence.mllib" ~dst_dir:"evidence";
    Pkg.lib "src/evidence_top_init.ml" ~dst:"evidence/";
    Pkg.mllib "samples/evidence_samples.mllib" ~dst_dir:"samples";
    Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
  ]
