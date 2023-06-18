(*---------------------------------------------------------------------------
   Copyright (c) 2023 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Gg_kit
open Brr_canvas

module C2d_style = struct
  let zero = Jstr.v "0"
  let hash = Jstr.v "#"
  let of_color c =
    let r, g, b, a = Color.to_srgbi c in
    let a = truncate (255. *. a +. 0.5) in
    let hex v =
      let hex = Jstr.of_int ~base:16 v in
      if Jstr.length hex = 1 then Jstr.(zero + hex) else hex
    in
    let c = Jstr.(hash + hex r + hex g + hex b + hex a) in
    C2d.color c
end

module C2d_path = struct
  let box2 b =
    let p = C2d.Path.create () in
    let x = Box2.ox b and y = Box2.oy b and w = Box2.w b and h = Box2.h b in
    C2d.Path.rect p ~x ~y ~w ~h;
    p

  let dot ~w pt =
    let p = C2d.Path.create () in
    let cx = P2.x pt and cy = P2.y pt in
    C2d.Path.arc p ~cx ~cy ~r:(0.5 *. w) ~start:0. ~stop:Float.two_pi;
    p

  let add_ring2 r p =
    let line pt first =
      if first
      then (C2d.Path.move_to p ~x:(V2.x pt) ~y:(V2.y pt); false)
      else (C2d.Path.line_to p ~x:(V2.x pt) ~y:(V2.y pt); false)
    in
    let _first = Ring2.fold_pts line r true in
    C2d.Path.close p; p

  let ring2 r = add_ring2 r (C2d.Path.create ())
  let pgon2 pgon =
    let p = C2d.Path.create () in
    Pgon2.fold_rings add_ring2 pgon p
end

module C2d_debug = struct
  let time label f =
    let label = Jstr.v label in
    Brr.Console.time label;
    Fun.protect ~finally:(fun () -> Brr.Console.time_end label) f

  let path_dir ~w = function
  | pt0 :: pt1 :: _ ->
      let hw = 0.5 *. w in
      let dir = V2.(unit (pt1 - pt0)) in
      let orthodir = V2.ortho dir in
      let hdir = V2.(hw * dir) in
      let horthodir = V2.(hw * orthodir) in
      let m = P2.mid pt0 pt1 in
      let a0 = V2.(m + hdir) in
      let ax = V2.(m - hdir) in
      let a1 = V2.(ax + horthodir) in
      let a2 = V2.(ax - horthodir) in
      let p = C2d.Path.create () in
      C2d.Path.move_to p ~x:(V2.x a0) ~y:(V2.y a0);
      C2d.Path.line_to p ~x:(V2.x a1) ~y:(V2.y a1);
      C2d.Path.line_to p ~x:(V2.x m) ~y:(V2.y m);
      C2d.Path.line_to p ~x:(V2.x a2) ~y:(V2.y a2);
      p
  | _ -> C2d.Path.create ()

end

let get_cnv ctx = match C2d.canvas ctx with
| None -> invalid_arg "Could not get canvas of context"
| Some cnv -> cnv

module C2d_viewrect = struct
  type h_align = [ `Left | `Center | `Right | `Fit ]
  type v_align = [ `Top | `Center | `Bottom | `Fit ]

  let aspect ctx =
    let cnv = get_cnv ctx in
    float (Canvas.w cnv) /. float (Canvas.h cnv)

  let set ?(h_align = `Left) ?(v_align = `Bottom) ctx ~view =
    let cnv = get_cnv ctx in
    let cw = float (Canvas.w cnv) and ch = float (Canvas.h cnv) in
    let vw = Box2.w view and vh = Box2.h view in
    let sx = cw /. vw and sy = ch /. vh in
    let set ?(padx = 0.) ?(pady = 0.) ~sx ~sy () =
      let e = padx +. -. Box2.ox view *. sx in
      let f = pady +. ch +. Box2.oy view *. sy in
      C2d.set_transform' ctx ~a:sx ~b:0. ~c:0. ~d:(~-.sy) ~e ~f
    in
    if Float.equal_tol ~eps:1.e-4 sx sy then set ~sx ~sy () else
    if sx < sy then match v_align with
    | `Fit -> set ~sx ~sy ()
    | `Top -> set ~pady:(-. (ch -. (vh *. sx))) ~sx ~sy:sx ()
    | `Center -> set ~pady:(-. (0.5 *. (ch -. (vh *. sx)))) ~sx ~sy:sx ()
    | `Bottom -> set ~sx ~sy:sx ()
    else match h_align with
    | `Fit -> set ~sx ~sy ()
    | `Left -> set ~sx:sy ~sy ()
    | `Center -> set ~padx:(0.5 *. (cw -. vw *. sy)) ~sx:sy ~sy ()
    | `Right -> set ~padx:(cw -. vw *. sy) ~sx:sy ~sy ()
end

module C2d_pixrect = struct
  let find_rect ~inv_tr cnv = function
  | Some rect -> rect
  | None ->
      let w = float (Canvas.w cnv) in
      let h = float (Canvas.h cnv) in
      let min = Vec4.tr inv_tr (Vec4.v ~x:0. ~y:0. ~z:0. ~w:1.) in
      let max = Vec4.tr inv_tr (Vec4.v ~x:w ~y:h ~z:0. ~w:1.) in
      let min = P2.v (Vec4.x min) (Vec4.y min) in
      let max = P2.v (Vec4.x max) (Vec4.y max) in
      Box2.of_pts min max

  let find_pixel_rect ~tr cnv = function
  | None -> 0, 0, (Canvas.w cnv), (Canvas.h cnv)
  | Some rect ->
      let tl = Box2.tl_pt rect in
      let br = Box2.br_pt rect in
      let tl = Vec4.tr tr (Vec4.v ~x:(P2.x tl) ~y:(P2.y tl) ~z:0. ~w:1.) in
      let br = Vec4.tr tr (Vec4.v ~x:(P2.x br) ~y:(P2.y br) ~z:0. ~w:1.) in
      let x = floor (Vec4.x tl) in
      let y = floor (Vec4.y tl) in
      let w = ceil (Vec4.x br) -. x in
      let h = ceil (Vec4.y br) -. y in
      (truncate x), (truncate y), (truncate w), (truncate h)

  type t =
    { image_data : C2d.Image_data.t;
      ix : int;
      iy : int;
      rect : box2 }

  let make ?color_space ?rect ctx =
    let tr = C2d.get_transform ctx in
    let inv_tr = Matrix4.inverse tr in
    let cnv = get_cnv ctx in
    let ix, iy, w, h = find_pixel_rect ~tr cnv rect in
    let rect = find_rect ~inv_tr cnv rect in
    let image_data = C2d.create_image_data ?color_space ctx ~w ~h in
    { image_data; ix; iy; rect }

  let get ?color_space ?rect ctx =
    let tr = C2d.get_transform ctx in
    let inv_tr = Matrix4.inverse tr in
    let cnv = get_cnv ctx in
    let ix, iy, w, h = find_pixel_rect ~tr cnv rect in
    let rect = find_rect ~inv_tr cnv rect in
    let image_data = C2d.get_image_data ?color_space ctx ~x:ix ~y:iy ~w ~h in
    { image_data; ix; iy; rect }

  let ix pr = pr.ix
  let iy pr = pr.iy
  let iw pr = C2d.Image_data.w pr.image_data
  let ih pr = C2d.Image_data.h pr.image_data
  let irect pr =
    let o = P2.v (float (ix pr)) (float (iy pr)) in
    let size = Size2.v (float (iw pr)) (float (ih pr)) in
    Box2.v o size

  let pixel_size pr =
    let pw = Box2.w pr.rect /. (float (iw pr)) in
    let ph = Box2.h pr.rect /. (float (ih pr)) in
    Size2.v pw ph

  let rect pr = pr.rect
  let ppu pr =
    let hres = (float (iw pr)) /. Box2.w pr.rect in
    let vres = (float (ih pr)) /. Box2.h pr.rect in
    Size2.v hres vres

  let color_space pr = C2d.Image_data.color_space pr.image_data
  let data pr = C2d.Image_data.data pr.image_data
  let image_data pr = pr.image_data

  let put ?o ctx pr =
    let x, y = match o with
    | None -> pr.ix, pr.iy
    | Some o ->
        let tr = C2d.get_transform ctx in
        let o = Vec4.tr tr (Vec4.v ~x:(P2.x o) ~y:(P2.y o) ~z:0. ~w:1.) in
        let x = floor (Vec4.x o) and bly = truncate (ceil (Vec4.y o)) in
        let y = bly - (C2d.Image_data.h pr.image_data) in
        (truncate x), y
    in
    C2d.put_image_data ctx ~x ~y pr.image_data
end

(*---------------------------------------------------------------------------
   Copyright (c) 2023 The vz programmers

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
