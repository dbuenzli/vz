(*---------------------------------------------------------------------------
   Copyright (c) 2023 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Brr
open Brr_canvas
open Fut.Syntax
open Gg
open Gg_kit
open Vz_canvas

let random () = Jv.to_float @@ Jv.call (Jv.get Jv.global "Math") "random" [||]

module Ball = struct
  type t =
    { size : float;
      x : float; y : float;
      vx : float; vy : float;  }

  let rand dom =
    let w = Box2.w dom and h = Box2.h dom in
    let size = 0.1 +. 0.1 *. w *. random () in
    let x = Box2.ox dom +. w *. random () in
    let y = Box2.oy dom +. h *. random () in
    let vx = 0.001 *. w *. random () in
    let vy = 0.001 *. h *. random () in
    { size; x; y; vx; vy; }

  let update dom s =
    let w = Box2.w dom and h = Box2.h dom in
    let x = s.x +. s.vx and y = s.y +. s.vy in
    let vx = if x > w -. s.size || x < s.size then -. s.vx else s.vx in
    let vy = if y > h -. s.size || y < s.size then -. s.vy else s.vy in
    { s with x; y; vx; vy }
end

let create_balls dom ~count = Array.init count (fun _ -> Ball.rand dom)
let update_balls dom balls =
  Array.iteri (fun i b -> balls.(i) <- Ball.update dom b) balls

let metaballs ?(dpu = 200.) dom balls =
  let meta balls pt =
    let acc = ref 0. in
    let px = V2.x pt and py = V2.y pt in
    for i = 0 to Array.length balls - 1 do
      let b = balls.(i) in
      let dx = px -. b.Ball.x and dy = py -. b.Ball.y in
      acc := !acc +. (b.Ball.size *. b.Ball.size) /. (dx *. dx +. dy *. dy)
    done;
    if !acc >= 1. then 1.0 else 0.0
  in
  let field = Field2.of_fun ~dpu ~dom (meta balls) in
  Field2.isoline ~smooth:true field 1.0

let draw_metaballs cnv ?dpu dom pgon =
  let ctx = C2d.get_context cnv in
  C2d_viewrect.set ctx ~v_align:`Top ~view:dom;
  C2d.set_fill_style ctx (C2d_style.of_color Color.black);
  C2d.fill_rect ctx
    ~x:(Box2.ox dom) ~y:(Box2.oy dom) ~w:(Box2.w dom) ~h:(Box2.h dom);
  C2d.set_line_width ctx 0.002;
  C2d.set_stroke_style ctx (C2d_style.of_color Color.white);
  C2d.stroke ctx (C2d_path.pgon2 pgon)

let draw_center ctx b =
  let p = C2d_path.dot ~w:0.02 (P2.v b.Ball.x  b.Ball.y) in
  C2d.set_fill_style ctx (C2d_style.of_color Color.white);
  C2d.fill ctx p

let animate cnv =
  let ctx = C2d.get_context cnv in
  let dom = Box2.v P2.o (Size2.v 2. 1.5) in
  let balls = create_balls dom ~count:5 in
  let rec loop _ =
    draw_metaballs cnv dom (metaballs dom balls);
    Array.iter (draw_center ctx) balls;
    update_balls dom balls;
    ignore (G.request_animation_frame loop)
  in
  loop 0.

let set_size cnv =
  let el = Canvas.to_el cnv in
  let w = El.inner_w el in
  let h = Jstr.(of_int (truncate ((w *. 3.) /. 4.)) + v "px") (* 4:3 *) in
  El.set_inline_style El.Style.height h el;
  Canvas.set_size_to_layout_size cnv

let main () =
  let h1 = El.h1 [El.txt' "Metaballs"] in
  let cnv = Canvas.create ~w:800 [] in
  El.append_children (Document.body G.document) [h1; Canvas.to_el cnv];
  let* _ev = Ev.next Ev.load (Window.as_target G.window) in
  set_size cnv; animate cnv; Fut.return ()

let () = ignore (main ())

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
