(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg

(* TODO code a proper Vz color scheme explorator with sliders etc. *)

let log fmt =
  let flush () = Js.string (Format.flush_str_formatter ()) in
  let flush _ = Firebug.console ## log (flush ()) in
  Format.kfprintf flush Format.str_formatter fmt

(* Color schemes to display. *)

let schemes = 
  let h = Float.rad_of_deg in
  [ `C (true, Vz.Colors.seq ~w:0. ~h:(h 255.) (), 9.);
    `D (true, Vz.Colors.seq_d ~w:0. ~h:(h 255.) 9); 
    `D (true, Vz.Colors.seq_d ~w:0.15 ~h:(h 255.) 9);
    `D (true, Vz.Colors.seq_d ~w:0.9 ~h:(h 255.) 9);
    `D (true, Vz.Colors.seq_d ~w:0.5 ~h:(h 10.) 9); 
    `D (true, Vz.Colors.seq_d ~h:(h 10.) 9); 
    `D (true, Vz.Colors.seq_d ~s:0.5 ~b:1. ~h:(h 255.) 9);
    `Blank; 
    `C (true, (Vz.Colors.div ~w:0.15 ~h0:(h 255.) ~h1:(h 10.) ()), 9.);
    `D (true, Vz.Colors.div_d ~w:0.15 ~h0:(h 255.) ~h1:(h 10.) 9); 
    `D (true, Vz.Colors.div_d ~m:0.6 ~w:0.15 ~h0:(h 255.) ~h1:(h 10.) 9); 
    `C (true, (Vz.Colors.div ~m:0.6 ~w:0.15 ~h0:(h 255.) ~h1:(h 10.) ()), 9.);
    `D (true, Vz.Colors.div_d ~w:0.15 ~h0:(h 255.) ~h1:(h 10.) 8); 
    `D (true, Vz.Colors.div_d ~w:0. ~h0:(h 255.) ~h1:(h 10.) 9); 
    `D (true, Vz.Colors.div_d ~w:1. ~s:0.7 ~h0:(h 120.) ~h1:(h 10.) 9);
    `D (true, Vz.Colors.div_d ~h0:(h 255.) ~h1:(h 10.) ~s:0.3 ~c:0.9 ~b:0.5 6);
    `Blank;
    `D (false, Vz.Colors.qual_fixed `Brewer_accent_8); 
    `D (false, Vz.Colors.qual_fixed `Brewer_dark2_8); 
    `D (false, Vz.Colors.qual_fixed `Brewer_paired_12); 
    `D (false, Vz.Colors.qual_fixed `Brewer_pastel1_9); 
    `D (false, Vz.Colors.qual_fixed `Brewer_pastel2_8); 
    `D (false, Vz.Colors.qual_fixed `Brewer_set1_9); 
    `D (false, Vz.Colors.qual_fixed `Brewer_set2_8); 
    `D (false, Vz.Colors.qual_fixed `Brewer_set3_12); 
    `D (false, Vz.Colors.qual_fixed `Wijffelaars_17); 
    `D (false, Vz.Colors.qual_d 8); 
    `D (false, Vz.Colors.qual_d ~c:0.8 8); ]

(* Continuous and discrete color schemes as images *)

let range ?(min = 0.) ?(max = 1.) dt f acc = 
  let n = truncate (((max -. min) /. dt) +. 1.) in
  let maxi = n - 1 in 
  let rec loop acc i = 
    if i < maxi then loop (f acc (min +. (float i) *. dt)) (i + 1) else 
    f acc max 
  in
  loop (f acc min) 1

let colors ?(rev = false) cs len =  (* scheme sampled every 0.05 unit of len *)
  let dt = 1. /. (floor ((len /. 0.05) +. 1.)) in
  let add_sample acc t = ((if rev then 1. -. t else t), cs t) :: acc in 
  let stops = 
    let stops = range dt add_sample [] in 
    if rev then stops else List.rev stops 
  in
  let bounds = P.empty >> P.rect (Box2.v P2.o (Size2.v 1. len)) in 
  I.axial stops P2.o (P2.v 0. len) >> I.cut bounds

let colors_d ?(rev = false) cs =                   (* discrete color scheme. *)
  let sq = 
    let sq = Box2.v P2.o (Size2.v 1. 1.01 (* overlap *)) in
    P.empty >> P.rect sq 
  in 
  let bounds = 
    let n = Array.length cs in
    P.empty >> P.rect (Box2.v P2.o (Size2.v 1. (float n))) 
  in
  let mv = P2.v 0. 1.0 in
  let add acc c = acc >> I.move mv >> I.blend (I.const c >> I.cut sq) in
  let colors = 
    if rev then Array.fold_left add I.void cs else
    Array.fold_right (fun c acc -> add acc c) cs I.void 
  in
  colors >> I.cut bounds (* cut topmost overlap *)
    
let size = Size2.v 200. 100. (* mm *)
let view = Box2.v P2.o (Size2.v 40. 20.)
let image =
  let add scheme acc =
    let i = match scheme with 
    | `D (rev, cs) -> colors_d ~rev cs 
    | `C (rev, cs, n) -> colors ~rev cs n 
    | `Blank -> I.void
    in
    acc >> I.move (P2.v 1.5 0.0) >> I.blend i
  in
  List.fold_right add schemes I.void >> 
  I.scale (V2.v 1. (-1.)) >> I.move (V2.v 0. 20.)
  
(* Browser bureaucracy. *)

let main _ = 
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
