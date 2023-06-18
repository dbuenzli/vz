(*---------------------------------------------------------------------------
   Copyright (c) 2023 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Gg_kit
open Brr
open Brr_canvas
open Fut.Syntax
open Vz
open Vz_canvas

(* TODO

   - Replace the vector rendering by Vg
   - Use Vz_doc to toggle various rendering bits.  *)

let render_field ctx field ~color =
  let img = C2d_pixrect.make ctx ~rect:(Field2.dom field) in
  let idata = C2d_pixrect.image_data img in
  let iw = C2d.Image_data.w idata and ih = C2d.Image_data.h idata in
  let pixels = Tarray.to_bigarray1 (C2d.Image_data.data idata) in
  Field2.to_rgba field ~color ~iw ~ih pixels ;
  C2d_pixrect.put ctx img

let render_ring_dir ctx ~w ~color r =
  C2d.set_fill_style ctx (C2d_style.of_color color);
  C2d.fill ctx (C2d_debug.path_dir ~w (Ring2.pts r))

let render_pgon_dirs ctx ~w ~color pgon =
  Pgon2.fold_rings (fun r () -> render_ring_dir ctx ~w ~color r) pgon ()

let render_pgon ctx ~w ~line_color ~color p =
  C2d.set_fill_style ctx (C2d_style.of_color color);
  C2d.fill ctx p;
  C2d.set_stroke_style ctx (C2d_style.of_color line_color);
  C2d.set_line_width ctx w;
  C2d.stroke ctx p;
  ()

let draw_field cnv ?dpu field =
  let field, levels, color = C2d_debug.time "Field computation" @@ field ?dpu in
  Console.(log ["Field scalars: "; Array.length (Field2.values field)]);
  let ctx = C2d.get_context cnv in
  let () = C2d_viewrect.set ctx ~v_align:`Top ~view:(Field2.dom field) in
  let () =
    if false then begin
    C2d_debug.time "Render_field" @@ fun () ->
    render_field ctx field ~color;
  end
  in
  let pgons =
    C2d_debug.time "Iso compute" @@ fun () ->
    let isolines iso = Field2.isoline field iso, color iso in
    List.map isolines levels
  in
  let () =
    C2d_debug.time "Iso render" @@ fun () ->
    let render_pgons (pgon, color) =
      let white = Color.gray 1.0 ~a:0.5 in
      let w = 0.0015 *. Box2.w (Field2.dom field) in
      render_pgon ctx ~w ~line_color:white ~color (C2d_path.pgon2 pgon);
      render_pgon_dirs ctx ~w:(5. *. w) ~color:white pgon;
    in
    List.iter render_pgons pgons
  in
  ()

let test_func ?(dpu = 400.) ()  =
  let func pt =
    let c = P2.v 0.5 0.5 in
    let d = V2.norm V2.(pt - c) in
    if 0.25 < d && d < 0.75
    then (if 0.375 < d && d < 0.625 then 0.0 else 1.0)
    else 0.0
  in
  let dom = Box2.v (P2.v (-2.) (-2.)) (Size2.v 4. 4.) in
  let field = Field2.of_fun ~dpu ~dom func in
  let levels = [1.0] in
  let color x =
    if x < 1.0
    then Color.of_oklch (V4.v 0.9 0.1 (Float.rad_of_deg 344.) 1.0)
    else Color.of_oklch (V4.v 0.9 0.1 (Float.rad_of_deg 150.) 1.0)
  in
  field, levels, color

let goldstein ?(dpu = 100.) () =
  (* Rip off of https://observablehq.com/@d3/contours *)
  let goldstein_price pt =
    (* https://en.wikipedia.org/wiki/Test_functions_for_optimization *)
    let x = P2.x pt and y = P2.y pt in
    let xx = x *. x and yy = y *. y and xy = x *. y in
    let c0 = x +. y +. 1. in
    let c0 = c0 *. c0 in
    let c1 = 19. -. 14. *. x +. 3. *. xx -. 14. *. y +. 6. *. xy +. 3. *. yy in
    let c2 = 2. *. x -. 3. *. y in
    let c2 = c2 *. c2 in
    let c3 =
      18. -. 32. *. x +. 12. *. xx +. 48. *. y -. 36. *. xy +. 27. *. yy
    in
    (1. +. c0 *. c1) *. (30. +. c2 *. c3)
  in
  let dom = Box2.v (P2.v (-2.) (-2.)) (Size2.v 4. 3.) in
  let field = Field2.of_fun ~dpu ~dom goldstein_price in
  let levels = List.init 19 (fun i -> 2. ** (float (i + 1))) in
(*  let levels = [List.nth levels 16 ] in *)
  let color =
    let magma = Color_scheme.sequential_magma () in
    let dom = Dom.log (List.hd levels) (List.hd (List.rev levels)) in
    fun v -> magma (Dom.inj dom v)
  in
  field, levels, color

let set_size cnv =
  let el = Canvas.to_el cnv in
  let w = El.inner_w el in
  let h = Jstr.(of_int (truncate ((w *. 3.) /. 4.)) + v "px") (* 4:3 *) in
  El.set_inline_style El.Style.height h el;
  Canvas.set_size_to_layout_size cnv

let main () =
  let h1 = El.h1 [El.txt' "Isolines"] in
  let cnv = Canvas.create ~w:800 [] in
  El.append_children (Document.body G.document) [h1; Canvas.to_el cnv];
  let* _ev = Ev.next Ev.load (Window.as_target G.window) in
  set_size cnv; draw_field cnv goldstein; Fut.return ()


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
