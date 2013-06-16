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


(* Data taken from 
   http://archive.ics.uci.edu/ml/datasets/Iris (bezdekIris.data) *)

let scatter ~size ~width ~pad ~x:(x, xdom) ~y:(y, ydom) ~color rows =
  let range = [ 0.5 *. pad; size -. 0.5 *. pad ] in
  let x = Scale.l_map (Scale.lin ~dom:xdom ~range:range) in
  let y = Scale.l_map (Scale.lin ~dom:ydom ~range:range) in
  let mark = Vz.Path.circle 3. in
  let dot r = I.const (color r) >> I.cut mark >> I.move (V2.v (x r) (y r)) in
  let blend_dot acc r = acc >> I.blend (dot r) in 
  List.fold_left blend_dot I.void rows

let species_col = "species", (fun (_, _, _, _, s) -> s)
let feature_cols = 
  [ "sepal length (cm)", (fun (l, _, _, _, _) -> l);
    "sepal width (cm)",  (fun (_, w, _, _, _) -> w);
    "petal length (cm)", (fun (_, _, l, _, _) -> l);
    "petal width (cm)",  (fun (_, _, _, w, _) -> w); ]  

let stat data = 
  let ranges = List.map (fun (_, col) -> Stat.range col) feature_cols in
  let species = Stat.range_d (snd species_col) in
  let stat = Stat.t2 (Stat.list ranges) species in 
  Stat.value (List.fold_left Stat.add stat data)

let image data = 
  let ranges, species = stat data in
  ignore (ranges, species);
  I.const (Color.v 0.314 0.784 0.471 1.)  

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
  assert(Vgr.render r (`Image (size, view, image data)) = `Ok); 
  a ## href <- (c ## toDataURL ());
  Js._false

and data =
[ 5.1, 3.5, 1.4, 0.2, "Iris setosa";
  4.9, 3.0, 1.4, 0.2, "Iris setosa";
  4.7, 3.2, 1.3, 0.2, "Iris setosa";
  4.6, 3.1, 1.5, 0.2, "Iris setosa";
  5.0, 3.6, 1.4, 0.2, "Iris setosa";
  5.4, 3.9, 1.7, 0.4, "Iris setosa";
  4.6, 3.4, 1.4, 0.3, "Iris setosa";
  5.0, 3.4, 1.5, 0.2, "Iris setosa";
  4.4, 2.9, 1.4, 0.2, "Iris setosa";
  4.9, 3.1, 1.5, 0.1, "Iris setosa";
  5.4, 3.7, 1.5, 0.2, "Iris setosa";
  4.8, 3.4, 1.6, 0.2, "Iris setosa";
  4.8, 3.0, 1.4, 0.1, "Iris setosa";
  4.3, 3.0, 1.1, 0.1, "Iris setosa";
  5.8, 4.0, 1.2, 0.2, "Iris setosa";
  5.7, 4.4, 1.5, 0.4, "Iris setosa";
  5.4, 3.9, 1.3, 0.4, "Iris setosa";
  5.1, 3.5, 1.4, 0.3, "Iris setosa";
  5.7, 3.8, 1.7, 0.3, "Iris setosa";
  5.1, 3.8, 1.5, 0.3, "Iris setosa";
  5.4, 3.4, 1.7, 0.2, "Iris setosa";
  5.1, 3.7, 1.5, 0.4, "Iris setosa";
  4.6, 3.6, 1.0, 0.2, "Iris setosa";
  5.1, 3.3, 1.7, 0.5, "Iris setosa";
  4.8, 3.4, 1.9, 0.2, "Iris setosa";
  5.0, 3.0, 1.6, 0.2, "Iris setosa";
  5.0, 3.4, 1.6, 0.4, "Iris setosa";
  5.2, 3.5, 1.5, 0.2, "Iris setosa";
  5.2, 3.4, 1.4, 0.2, "Iris setosa";
  4.7, 3.2, 1.6, 0.2, "Iris setosa";
  4.8, 3.1, 1.6, 0.2, "Iris setosa";
  5.4, 3.4, 1.5, 0.4, "Iris setosa";
  5.2, 4.1, 1.5, 0.1, "Iris setosa";
  5.5, 4.2, 1.4, 0.2, "Iris setosa";
  4.9, 3.1, 1.5, 0.2, "Iris setosa";
  5.0, 3.2, 1.2, 0.2, "Iris setosa";
  5.5, 3.5, 1.3, 0.2, "Iris setosa";
  4.9, 3.6, 1.4, 0.1, "Iris setosa";
  4.4, 3.0, 1.3, 0.2, "Iris setosa";
  5.1, 3.4, 1.5, 0.2, "Iris setosa";
  5.0, 3.5, 1.3, 0.3, "Iris setosa";
  4.5, 2.3, 1.3, 0.3, "Iris setosa";
  4.4, 3.2, 1.3, 0.2, "Iris setosa";
  5.0, 3.5, 1.6, 0.6, "Iris setosa";
  5.1, 3.8, 1.9, 0.4, "Iris setosa";
  4.8, 3.0, 1.4, 0.3, "Iris setosa";
  5.1, 3.8, 1.6, 0.2, "Iris setosa";
  4.6, 3.2, 1.4, 0.2, "Iris setosa";
  5.3, 3.7, 1.5, 0.2, "Iris setosa";
  5.0, 3.3, 1.4, 0.2, "Iris setosa";
  7.0, 3.2, 4.7, 1.4, "Iris versicolor";
  6.4, 3.2, 4.5, 1.5, "Iris versicolor";
  6.9, 3.1, 4.9, 1.5, "Iris versicolor";
  5.5, 2.3, 4.0, 1.3, "Iris versicolor";
  6.5, 2.8, 4.6, 1.5, "Iris versicolor";
  5.7, 2.8, 4.5, 1.3, "Iris versicolor";
  6.3, 3.3, 4.7, 1.6, "Iris versicolor";
  4.9, 2.4, 3.3, 1.0, "Iris versicolor";
  6.6, 2.9, 4.6, 1.3, "Iris versicolor";
  5.2, 2.7, 3.9, 1.4, "Iris versicolor";
  5.0, 2.0, 3.5, 1.0, "Iris versicolor";
  5.9, 3.0, 4.2, 1.5, "Iris versicolor";
  6.0, 2.2, 4.0, 1.0, "Iris versicolor";
  6.1, 2.9, 4.7, 1.4, "Iris versicolor";
  5.6, 2.9, 3.6, 1.3, "Iris versicolor";
  6.7, 3.1, 4.4, 1.4, "Iris versicolor";
  5.6, 3.0, 4.5, 1.5, "Iris versicolor";
  5.8, 2.7, 4.1, 1.0, "Iris versicolor";
  6.2, 2.2, 4.5, 1.5, "Iris versicolor";
  5.6, 2.5, 3.9, 1.1, "Iris versicolor";
  5.9, 3.2, 4.8, 1.8, "Iris versicolor";
  6.1, 2.8, 4.0, 1.3, "Iris versicolor";
  6.3, 2.5, 4.9, 1.5, "Iris versicolor";
  6.1, 2.8, 4.7, 1.2, "Iris versicolor";
  6.4, 2.9, 4.3, 1.3, "Iris versicolor";
  6.6, 3.0, 4.4, 1.4, "Iris versicolor";
  6.8, 2.8, 4.8, 1.4, "Iris versicolor";
  6.7, 3.0, 5.0, 1.7, "Iris versicolor";
  6.0, 2.9, 4.5, 1.5, "Iris versicolor";
  5.7, 2.6, 3.5, 1.0, "Iris versicolor";
  5.5, 2.4, 3.8, 1.1, "Iris versicolor";
  5.5, 2.4, 3.7, 1.0, "Iris versicolor";
  5.8, 2.7, 3.9, 1.2, "Iris versicolor";
  6.0, 2.7, 5.1, 1.6, "Iris versicolor";
  5.4, 3.0, 4.5, 1.5, "Iris versicolor";
  6.0, 3.4, 4.5, 1.6, "Iris versicolor";
  6.7, 3.1, 4.7, 1.5, "Iris versicolor";
  6.3, 2.3, 4.4, 1.3, "Iris versicolor";
  5.6, 3.0, 4.1, 1.3, "Iris versicolor";
  5.5, 2.5, 4.0, 1.3, "Iris versicolor";
  5.5, 2.6, 4.4, 1.2, "Iris versicolor";
  6.1, 3.0, 4.6, 1.4, "Iris versicolor";
  5.8, 2.6, 4.0, 1.2, "Iris versicolor";
  5.0, 2.3, 3.3, 1.0, "Iris versicolor";
  5.6, 2.7, 4.2, 1.3, "Iris versicolor";
  5.7, 3.0, 4.2, 1.2, "Iris versicolor";
  5.7, 2.9, 4.2, 1.3, "Iris versicolor";
  6.2, 2.9, 4.3, 1.3, "Iris versicolor";
  5.1, 2.5, 3.0, 1.1, "Iris versicolor";
  5.7, 2.8, 4.1, 1.3, "Iris versicolor";
  6.3, 3.3, 6.0, 2.5, "Iris virginica";
  5.8, 2.7, 5.1, 1.9, "Iris virginica";
  7.1, 3.0, 5.9, 2.1, "Iris virginica";
  6.3, 2.9, 5.6, 1.8, "Iris virginica";
  6.5, 3.0, 5.8, 2.2, "Iris virginica";
  7.6, 3.0, 6.6, 2.1, "Iris virginica";
  4.9, 2.5, 4.5, 1.7, "Iris virginica";
  7.3, 2.9, 6.3, 1.8, "Iris virginica";
  6.7, 2.5, 5.8, 1.8, "Iris virginica";
  7.2, 3.6, 6.1, 2.5, "Iris virginica";
  6.5, 3.2, 5.1, 2.0, "Iris virginica";
  6.4, 2.7, 5.3, 1.9, "Iris virginica";
  6.8, 3.0, 5.5, 2.1, "Iris virginica";
  5.7, 2.5, 5.0, 2.0, "Iris virginica";
  5.8, 2.8, 5.1, 2.4, "Iris virginica";
  6.4, 3.2, 5.3, 2.3, "Iris virginica";
  6.5, 3.0, 5.5, 1.8, "Iris virginica";
  7.7, 3.8, 6.7, 2.2, "Iris virginica";
  7.7, 2.6, 6.9, 2.3, "Iris virginica";
  6.0, 2.2, 5.0, 1.5, "Iris virginica";
  6.9, 3.2, 5.7, 2.3, "Iris virginica";
  5.6, 2.8, 4.9, 2.0, "Iris virginica";
  7.7, 2.8, 6.7, 2.0, "Iris virginica";
  6.3, 2.7, 4.9, 1.8, "Iris virginica";
  6.7, 3.3, 5.7, 2.1, "Iris virginica";
  7.2, 3.2, 6.0, 1.8, "Iris virginica";
  6.2, 2.8, 4.8, 1.8, "Iris virginica";
  6.1, 3.0, 4.9, 1.8, "Iris virginica";
  6.4, 2.8, 5.6, 2.1, "Iris virginica";
  7.2, 3.0, 5.8, 1.6, "Iris virginica";
  7.4, 2.8, 6.1, 1.9, "Iris virginica";
  7.9, 3.8, 6.4, 2.0, "Iris virginica";
  6.4, 2.8, 5.6, 2.2, "Iris virginica";
  6.3, 2.8, 5.1, 1.5, "Iris virginica";
  6.1, 2.6, 5.6, 1.4, "Iris virginica";
  7.7, 3.0, 6.1, 2.3, "Iris virginica";
  6.3, 3.4, 5.6, 2.4, "Iris virginica";
  6.4, 3.1, 5.5, 1.8, "Iris virginica";
  6.0, 3.0, 4.8, 1.8, "Iris virginica";
  6.9, 3.1, 5.4, 2.1, "Iris virginica";
  6.7, 3.1, 5.6, 2.4, "Iris virginica";
  6.9, 3.1, 5.1, 2.3, "Iris virginica";
  5.8, 2.7, 5.1, 1.9, "Iris virginica";
  6.8, 3.2, 5.9, 2.3, "Iris virginica";
  6.7, 3.3, 5.7, 2.5, "Iris virginica";
  6.7, 3.0, 5.2, 2.3, "Iris virginica";
  6.3, 2.5, 5.0, 1.9, "Iris virginica";
  6.5, 3.0, 5.2, 2.0, "Iris virginica";
  6.2, 3.4, 5.4, 2.3, "Iris virginica";
  5.9, 3.0, 5.1, 1.8, "Iris virginica"; ]

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
