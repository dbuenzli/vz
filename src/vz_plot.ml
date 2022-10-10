(*---------------------------------------------------------------------------
   Copyright (c) 2022 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Gg_kit
open Vg
open Vz
open Evidence

let log fmt = Format.printf (fmt ^^"@.")

module Vmap = struct
  type kind = [ `Qual | `Quant ]

  type ('o, 'a) dom =
  | Dom of 'a Dom.t
  | Dom_deriver of
      (kind -> 'o Dataset.t -> ('o, 'a) Var.t -> 'a Dom.Shape.t -> 'a Dom.t)

  type 'a range =
  | Range of 'a Dom.t
  | Range_deriver of (kind -> w:float -> 'a Dom.t -> 'a Dom.t) (** *)

  type 'o var =
  | Var : ('o, 'a) Var.t * 'a Dom.Shape.t option * ('o, 'a) dom -> 'o var

  type ('o, 'a) vmap =
    { kind : kind;
      var : 'o var;
      range : 'a range; }

  type ('o, 'a) t = Vmap of ('o, 'a) vmap | Const of 'a


  let v ?shape kind var dom range =
    Vmap { kind; var = Var (var, shape, dom); range }

  let const v = Const v

  let v' dom_deriver range_deriver ?shape ?dom ?range kind var =
    let dom = match dom with
    | None -> Dom_deriver dom_deriver | Some dom -> Dom dom
    in
    let var = Var (var, shape, dom) in
    let range = match range with
    | None -> Range_deriver range_deriver | Some range -> Range range
    in
    Vmap { kind; var; range }

  let mag = (* FIXME generalize *)
    let dom_deriver _k d var _shape =
      let min, max = Dataset.Var.min_max var d in
      Dom.linear min max
    in
    let range_deriver _k ~w _dom = Dom.linear 0. w in
    fun ?shape ?dom ?range kind var ->
      v' dom_deriver range_deriver ?shape ?dom ?range kind var

  let area =
    let dom_deriver _k d var _shape =
      let min, max = Dataset.Var.min_max var d in
      Dom.linear min max
    in
    let range_deriver _k ~w _dom = Dom.linear 0. w in
    fun ?(shape = Dom.Shape.sqrt) ?dom ?range kind var ->
      v' dom_deriver range_deriver ~shape  ?dom ?range kind var

  let color =
    let dom_deriver _k d var _shape =
      let min, max = Dataset.Var.min_max var d in
      Dom.linear min max
    in
    let range_deriver _k ~w _dom =
      Dom.v (Dom.Continuous Uniform) []
        ~none:(Color.v nan nan nan nan)
        (Fun.const Float.nan) (Color_scheme.sequential_turbo ())
    in
    fun ?(shape = Dom.Shape.sqrt) ?dom ?range kind var ->
      v' dom_deriver range_deriver ~shape ?dom ?range kind var

  let min_max v d = 0., 0.
  let app v d = 1.
end

module Marks = struct
  type 'o dot =
    { r : ('o, float) Vmap.t;
      x : ('o, float) Vmap.t;
      y : ('o, float) Vmap.t;
      fill : ('o, Color.t) Vmap.t;
      outline : ('o, Color.t) Vmap.t;
      to_image : w:float -> 'o Dataset.t -> Box2.t * Vg.image; }

  type 'o t =
  | Dot of 'o dot

  let dot_path r = P.circle P2.o r P.empty
  let dot_outline r =
    let area = `O { P.o with P.width = 0.04 } in
    let black = I.const Color.black in
    I.cut ~area (dot_path r) black

  let dot r = I.cut (dot_path r) @@ I.const Color.black
  let dots r x y fill outline ~w d =
    let xmin, xmax = Vmap.min_max x d in
    let ymin, ymax = Vmap.min_max y d in
    let xrange = xmax -. xmin in
    let yrange = ymax -. ymin in
    let aspect = xrange /. yrange in
    let size = Size2.of_w w ~aspect in
    let sx = Size2.w size /. xrange in
    let sy = Size2.h size /. yrange in
    let add_mark _ o acc =
      let x = (Vmap.app x o -. xmin) *. sx in
      let y = (Vmap.app y o -. ymin) *. sy in
      log "%a" V2.pp (P2.v x y);
      I.blend (I.move (P2.v x y) (dot (Vmap.app r o))) acc
    in
    let marks = Dataset.fold add_mark d I.void in
    log "size:%a, xmin:%f xmax:%f ymin:%f ymax:%f scale:%f %f" V2.pp size
      xmin xmax ymin ymax sx sy;
    Box2.v P2.o size, marks

  let dot ?r ?x ?y ?fill:f ?outline:o () =
    let r = match r with None -> Vmap.const 1. | Some r -> r in
    let x = match x with None -> Vmap.const 0. | Some x -> x in
    let y = match y with None -> Vmap.const 0. | Some y -> y in
    let fill = match f with None -> Vmap.const Color.black | Some y -> y in
    let outline = match o with None -> Vmap.const Color.black | Some y -> y in
    let to_image = dots r x y fill outline in
    Dot { r; x; y; fill; outline; to_image }

  let to_image m = match m with
  | Dot dot -> dot.to_image
end

module Plot = struct
  type marks = M : ('a Marks.t * 'a Dataset.t) list -> marks
  type t = { w : float; h : float option; marks : marks; }

  let marks ~w (m, d) = Marks.to_image m ~w d
  let v ?(w = 160.) ?h ms = { w; h; marks = M ms }
  let empty = { w = 0.; h = None; marks = M [] }
  let over p0 p1 = failwith "TODO"
  let box p =
    let h = match p.h with None -> p.w | Some h -> h in
    Box2.v P2.o (Size2.v p.w h)

  let image p = I.const Color.red
  let to_image p =
    let M ms = p.marks in
    marks ~w:p.w (List.hd ms)
end

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
