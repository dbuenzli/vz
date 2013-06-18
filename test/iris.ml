(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vz

(* Define your image *)

let aspect = 1.618  
let size = Size2.v (aspect *. 100.) 100. (* mm *)
let view = Box2.v P2.o (Size2.v aspect 1.)

let log fmt =
  let flush () = Js.string (Format.flush_str_formatter ()) in
  let flush _ = Firebug.console ## log (flush ()) in
  Format.kfprintf flush Format.str_formatter fmt

let xyset x y = (* indexed X x Y *)
  let xs xi x = List.mapi (fun yi y -> ((float xi), x), ((float yi), x)) y in
  List.concat (List.mapi xs x)

let xyplot ~pad ~size ~x:((xl, x), xdom) ~y:((yl, y), ydom) ~color data =
  let range = [ 0.5 *. pad; size -. 0.5 *. pad ] in
  let x = Scale.l_map (Scale.lin ~dom:xdom ~range:range) in
  let y = Scale.l_map (Scale.lin ~dom:ydom ~range:range) in
  let mark = Vz.Path.circle 3. in
  let dot v = I.const (color v) >> I.cut mark >> I.move (V2.v (x v) (y v)) in
  let blend_dot acc r = acc >> I.blend (dot r) in 
  List.fold_left blend_dot I.void data

let specie = "species", (fun (_, _, _, _, s) -> s)
let traits = 
  [ "sepal length (cm)", (fun (l, _, _, _, _) -> l);
    "sepal width (cm)",  (fun (_, w, _, _, _) -> w);
    "petal length (cm)", (fun (_, _, l, _, _) -> l);
    "petal width (cm)",  (fun (_, _, _, w, _) -> w); ]  

let stats data = 
  let trait_ranges = List.map (fun col -> Stat.range (snd col)) traits in
  let species = Stat.range_d (snd specie) in
  let stats = Stat.t2 (Stat.list trait_ranges) species in 
  Stat.value (List.fold_left Stat.add stats data)
  
let image = 
  let size = 35. in
  let pad = 5. in
  let trait_ranges, species = stats Iris_data.sample in
  let traits = List.combine traits trait_ranges in 
  let xys = xyset traits traits in
  let add_xy acc ((xi, x), (yi, y)) = 
    let m = V2.v (xi *. size) (yi *. size) in
    I.void
(*
    acc I.blend ((xyplot ~pad ~size ~x ~y) >> I.move m) *)
  in
  List.fold_left add_xy I.void xys

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
  assert(Vgr.render r (`Image (size, view, image)) = `Ok); 
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
