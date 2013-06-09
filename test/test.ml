(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg

let str = Format.sprintf
let log f = Format.printf (f ^^ "@?") 
let fail fmt = 
  let fail _ = failwith (Format.flush_str_formatter ()) in
  Format.kfprintf fail Format.str_formatter fmt
  
let irange ?(min = 0.) ?(max = 1.) ~dt f = 
  let n = truncate (((max -. min) /. dt) +. 1.) in
  let maxi = n - 1 in 
  let rec loop i = 
    if i < maxi then (f (min +. (float i) *. dt); loop (i + 1)) else f max
  in
  if max = min then f max else
  (f min; loop 1)

let ( >>= ) f x = f x

(* internal function, test passed 

let test_msc () = 
  let test_edge c =
    irange ~min:0. ~max:1. ~dt:0.01 >>= fun t ->
    let c = c t in
    let _, _, h, _ = V4.to_tuple (Color.to_luva ~lch:true c) in 
    let c' = Vz.Colors.msc h in 
    if not (V4.equal_f (Float.equal_tol ~eps:1.e-9) c c') then 
      fail "%a != %a\n" V4.pp c V4.pp c';
  in
  test_edge (fun t -> Color.v t  1. 0. 1.);
  test_edge (fun t -> Color.v t  0. 1. 1.);
  test_edge (fun t -> Color.v 0. t  1. 1.);
  test_edge (fun t -> Color.v 1. t  0. 1.);
  test_edge (fun t -> Color.v 0. 1. t  1.);
  test_edge (fun t -> Color.v 1. 0. t  1.)
*)  

let test_color_seq () = 
  log "Testing sequential color schemes do not NaN.\n";
  irange ~min:0. ~max:359. ~dt:1. >>= fun h ->
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun w ->
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun s ->
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun b -> 
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun c ->
  let cs = Vz.Colors.seq ~w ~s ~b ~c ~h:(Float.rad_of_deg h) () in 
  irange ~min:0. ~max:1. ~dt:1. >>= fun t -> 
  let color = cs t in 
  let urange d = 0. <= d && d <= 1. in
  if V4.for_all urange color then () else
  let cr, cg, cb, ca = V4.to_tuple color in
  fail "not in rgb cube w:%g s:%g b:%g c:%g h:%g t:%g \
        (%.16f %.16f %.16f)" 
    w s b c h t cr cg cb

let test_qual () = 
  log "Testing qualitative color schemes do not NaN.\n";
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun eps -> 
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun r -> 
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun s -> 
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun b -> 
  irange ~min:0. ~max:1. ~dt:0.1 >>= fun c ->
  let q = Vz.Colors.qual_d 16 in
  for i = 0 to Array.length q - 1 do 
    let color = q.(i) in
    let urange d = 0. <= d && d <= 1. in 
    if V4.for_all urange color then () else
    let cr, cg, cb, _ = V4.to_tuple color in
    fail "qualitative color not in rgb cube eps:%g r:%g s:%g b:%g c:%g \
          (%.16f %.16f %.16f)" 
      eps r s b c cr cg cb
  done
  
let test () =
  Printexc.record_backtrace true; 
  test_color_seq (); 
  test_qual ();
  log "All tests succeded.\n"

let () = if not (!Sys.interactive) then test () 
 

(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%
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
