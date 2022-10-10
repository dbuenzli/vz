(*---------------------------------------------------------------------------
   Copyright (c) 2022 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vz
open Vz_plot
open Evidence


let log fmt = Format.printf (fmt ^^ "@.")

let dots =
  let x = Vmap.mag `Quant Evidence_iris.sepal_length in
  let y = Vmap.mag `Quant Evidence_iris.sepal_width in
  Marks.dot ~x ~y ()

let p = Plot.v [dots, Evidence_iris.dataset]

let view, image = Plot.to_image p
let image = `Image (Box2.size view, view, image)

(* Browser bureaucracy. *)

open Brr
open Brr_canvas

let main () =
  let c =
    let c = Canvas.create [] in
    El.append_children (Document.body G.document) [Canvas.to_el c]; c
  in
  let r =
    let t = Vgr_htmlc.target ~resize:true (Obj.magic c) in
    Vg.Vgr.create t `Other
  in
  assert(Vgr.render r image = `Ok);
  ()

let () = main ()



(*---------------------------------------------------------------------------
   Copyright (c) 2022 The vz programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
