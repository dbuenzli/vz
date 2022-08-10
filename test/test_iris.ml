(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vz

open Brr
open Brr_canvas

let log fmt = Format.printf (fmt ^^ "@.")

(* Style *)

let font =
  { Font.name = "OpenSans"; size = 3.25; slant = `Normal; weight = `W400 }

let lgray = I.const (Color.gray 0.75)

(* Data *)

let data = Evidence.Dataset.to_list Evidence_iris.dataset
let label = fst
let getter = snd
let species = "species", (fun (_, _, _, _, s) -> s)
let traits =
  [ "sepal length (cm)", (fun (l, _, _, _, _) -> l);
    "sepal width (cm)",  (fun (_, w, _, _, _) -> w);
    "petal length (cm)", (fun (_, _, l, _, _) -> l);
    "petal width (cm)",  (fun (_, _, _, w, _) -> w); ]

let corners size box p = (* path for ⌜ ⌝ ⌞ ⌟ *)
  let dx, neg_dx = V2.v size 0., V2.v (-. size) 0. in
  let dy, neg_dy = V2.v 0. size, V2.v 0. (-. size) in
  let minx, maxx = Box2.minx box, Box2.maxx box in
  let miny, maxy = Box2.miny box, Box2.maxy box in
  let line dv = P.line ~rel:true dv in
  p |>
  P.sub (P2.v minx (maxy -. size)) |> line dy |> line dx |>
  P.sub (P2.v (maxx -. size) maxy) |> line dx |> line neg_dy |>
  P.sub (P2.v maxx (miny +. size)) |> line neg_dy |> line neg_dx |>
  P.sub (P2.v (minx +. size) miny) |> line neg_dx |> line dy

let species_stats data =
  let column_range col = Stat.range (getter col) in
  let trait_ranges = List.map column_range traits in
  let species = Stat.range_d (getter species) in
  let stats = Stat.t2 (Stat.list trait_ranges) species in
  Stat.value (List.fold_left Stat.add stats data)

let xyplot ~pad ~size ~x ~y obj color data acc =
  let mark = I.cut (Mark.dot 1.0) in
  let dot v = I.const (color (obj v)) |> mark |> I.move (V2.v (x v) (y v)) in
  let blend_dot acc r = acc |> I.blend (dot r) in
  List.fold_left blend_dot acc data

let xyset_scales ~pad ~size x y =
  let range = 0.5 *. pad, size -. 0.5 *. pad in
  let xy_scale xi (x_col, x_dom) yi (y_col, y_dom) =
    ((float xi), x_col, Scale'.linear x_dom range),
    ((float yi), y_col, Scale'.linear y_dom range)
  in
  let xs xi x = List.mapi (xy_scale xi x) y in
  List.concat (List.mapi xs x)

let image =
  let trait_count = List.length traits in
  let trait_ranges, species_range = species_stats data in
  let traits = List.combine traits trait_ranges in
  let species_count = List.length species_range in
  let image_size = 160. in
  let size = (image_size /. (float trait_count)) in
  let pad = size *. 0.1125 in
  let colors = Colors.qual_fixed ~a:0.8 ~size:species_count `Brewer_set2_8 in
  let cmap = Scale'.map (Scale'.ordinal species_range (Array.to_list colors)) in
  let xyset_scales = xyset_scales ~pad ~size traits traits in
  let add_ticks acc ((xi, _, xscale), (yi, _, yscale)) =
    let xticks =
      if yi <> 0. then I.void else
      let x = Scale'.map xscale in
      let _y = Scale'.map yscale in
      let add_tick acc prec t =
        let label = Printf.sprintf "%.*f" prec t in
        log "%s" label;
        let pos = V2.v (x t) (0.4 *. pad) in
        let area = `O { P.o with P.width = 0.2; } in
        acc |>
        I.blend (I.cut ~area (Mark.vtick ~pos ~valign:`Center 2.0) lgray)
(*        I.blend (I.cut_glyphs *)
      in
      let pos = V2.v (xi *. size) 0. in
      Scale'.fold_ticks ~bounds:false 5 add_tick I.void xscale |> I.move pos
    in
    let yticks =
      if xi <> 0. then I.void else
      let _x = Scale'.map xscale in
      let y = Scale'.map yscale in
      let add_tick acc prec t =
        log "%.*f" prec t;
        let pos = V2.v (0. *. pad) (y t) in
        let area = `O { P.o with P.width = 0.2; } in
        acc |>
        I.blend (I.cut ~area (Mark.htick ~pos ~halign:`Center 2.0) lgray)
      in
      let pos = V2.v (0.) (yi *. size) in
      Scale'.fold_ticks ~bounds:false 5 add_tick I.void yscale |> I.move pos
    in
    acc |> I.blend xticks |> I.blend yticks
  in
  let add_xy acc ((xi, x_col, xscale), (yi, y_col, yscale)) =
    let hpad, _ as range = 0.5 *. pad, size -. 0.5 *. pad in
    let frame =
      let s = size -. pad in
      let bounds = Box2.v (P2.v hpad hpad) (Size2.v s s) in
      let pbounds = P.empty |> corners pad bounds in
      let area = `O { P.o with P.width = 0.15; } in
      let pbounds = pbounds |> P.rect bounds in
      I.cut ~area pbounds lgray
    in
    let xmap = Scale'.map xscale in
    let ymap = Scale'.map yscale in
    let x v = xmap ((getter x_col) v) in
    let y v = ymap ((getter y_col) v) in
    let pos = V2.v (xi *. size) (image_size -. (yi +. 1.) *. size) in
    let img =
      if xi <> yi
      then
        xyplot ~pad ~size ~x ~y (getter species) cmap data I.void
      else
      let lpos = V2.v (1.6 *. pad) (0.5 *. (size -. font.Font.size)) in
      (I.const Color.black) |>
      I.cut_glyphs ~text:(label x_col) font [] |> I.move lpos
    in
    acc |> I.blend (frame |> I.blend img |> I.move pos)
  in
  let grid = List.fold_left add_ticks I.void xyset_scales in
  let image = List.fold_left add_xy grid xyset_scales in
  let view = Box2.v P2.o (Size2.v image_size image_size) in
  let size = Size2.v image_size image_size (* mm *) in
  `Image (size, view, image)

(* Browser bureaucracy. *)

let main () =
  let body = (Document.body G.document) in
  let c = Canvas.create [] in
  let () = El.append_children body [Canvas.to_el c] in
  let r =
    let t = Vgr_htmlc.target (* ~resize:true *) (Obj.magic c) in
    Vg.Vgr.create t `Other
  in
  assert(Vgr.render r image = `Ok);
  ()

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vz programmers

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
