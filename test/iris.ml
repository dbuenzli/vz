(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vz

let log fmt =
  let flush () = Js.string (Format.flush_str_formatter ()) in
  let flush _ = Firebug.console ## log (flush ()) in
  Format.kfprintf flush Format.str_formatter fmt

let label = fst
let getter = snd
let species = "species", (fun (_, _, _, _, s) -> s)
let traits = List.rev
  [ "sepal length (cm)", (fun (l, _, _, _, _) -> l);
    "sepal width (cm)",  (fun (_, w, _, _, _) -> w);
    "petal length (cm)", (fun (_, _, l, _, _) -> l);
    "petal width (cm)",  (fun (_, _, _, w, _) -> w); ]  

let species_stats data = 
  let column_range col = Stat.range (getter col) in
  let trait_ranges = List.map column_range traits in
  let species = Stat.range_d (getter species) in
  let stats = Stat.t2 (Stat.list trait_ranges) species in 
  Stat.value (List.fold_left Stat.add stats data)

let xyplot ~pad ~size ~x ~y obj color data acc =
  let mark = I.cut (Mark.dot 1.0) in
  let dot v = I.const (color (obj v)) >> mark >> I.move (V2.v (x v) (y v)) in
  let blend_dot acc r = acc >> I.blend (dot r) in 
  List.fold_left blend_dot acc data

let xyset_scales ~pad ~size x y =
  let range = 0.5 *. pad, size -. 0.5 *. pad in
  let xy_scale xi (x_col, x_dom) yi (y_col, y_dom) =
    ((float xi), x_col, Scale.linear x_dom range),
    ((float yi), y_col, Scale.linear y_dom range)
  in
  let xs xi x = List.mapi (xy_scale xi x) y in
  List.concat (List.mapi xs x)

let font = Font.create "OpenSans" 2.2
let labeli col = I.cut_glyphs ~text:(label col) font [] (I.const Color.black) 

let image =
  let size = 35. in
  let pad = 4.5 in
  let trait_count = List.length traits in
  let trait_ranges, species_range = species_stats Iris_data.sample in
  let traits = List.combine traits trait_ranges in
  let species_count = List.length species_range in
  let colors = Colors.qual_fixed ~a:0.8 ~size:species_count `Brewer_set2_8 in
  let cmap = Scale.map (Scale.ordinal species_range (Array.to_list colors)) in
  let xyset_scales = xyset_scales ~pad ~size traits traits in
  let add_ticks acc ((xi, _, xscale), (yi, _, yscale)) = 
    let xticks = if xi <> 0. then I.void else I.void in
    let yticks = if yi <> 0. then I.void else I.void in
    acc >> I.blend xticks >> I.blend yticks
  in
  let add_xy acc ((xi, x_col, xscale), (yi, y_col, yscale)) =
    let hpad, _ as range = 0.5 *. pad, size -. 0.5 *. pad in
    let frame = 
      let s = size -. pad in
      let bounds = P.empty >> P.rect (Box2.v (P2.v hpad hpad) (Size2.v s s)) in
      let area = `O { P.o with P.width = 0.2; } in
      I.cut ~area bounds (I.const (Color.gray 0.75))
    in
    let xmap = Scale.map xscale in
    let ymap = Scale.map yscale in
    let x v = xmap ((getter x_col) v) in 
    let y v = ymap ((getter y_col) v) in
    let pos = V2.v (xi *. size) (yi *. size) in
    let plot = xyplot ~pad ~size ~x ~y (getter species) cmap Iris_data.sample in
    let lpos = V2.v (V2.x pos +. pad) (V2.y pos +. size -. 1.5 *. pad) in
    let label = if xi = yi then labeli x_col else I.void in 
    acc >> I.blend (frame >> plot >> I.move pos) >> 
    I.blend (label >> I.move lpos)
  in
  let grid = List.fold_left add_ticks I.void xyset in
  let image = List.fold_left add_xy grid xyset_scales in
  let side = size *. (float trait_count) +. pad in
  let view = Box2.v P2.o (Size2.v (side +. size) side) in 
  let size = Size2.v 200. 160. (* mm *) in 
  `Image (size, view, image) 
  
(* Browser bureaucracy. *)

let rec main _ = 
  let d = Dom_html.window ## document in 
  let a = 
    let a = Dom_html.createA d in 
    a ## title <- Js.string "Download PNG file";
    a ## href <- Js.string "#"; 
    a ## setAttribute (Js.string "download", Js.string "minc.png");
    Dom.appendChild (d ## body) a; a
  in 
  let c = 
    let c = Dom_html.createCanvas d in 
    Dom.appendChild a c; c
  in 
  let r = Vgr.create (Vgr_htmlc.target c) `Other in 
  assert(Vgr.render r image = `Ok); 
  a ## href <- (c ## toDataURL ());
  Js._false

let () = Dom_html.window ## onload <- Dom_html.handler main

(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
