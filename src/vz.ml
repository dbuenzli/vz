(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%

   ColorBrewer Color Schemes are Copyright 2002 Cynthia Brewer, Mark Harrower
   and the Pennsylvania State University. Licensed under the Apache License
   version 2.0, see http://www.apache.org/licenses/LICENSE-2.0
  ---------------------------------------------------------------------------*)

open Gg
open Vg

let err_scheme_size ssize s = 
  Printf.sprintf "scheme size %d exceeded (%d)" ssize s

module Color = struct

  (* Sequential color schemes *)

  let seq ?(w = 0.) ?(s = 0.6) ?(b = 0.75) ?(c = 0.88) ~h = 
    failwith "TODO"

  let seq_d ?w ?s ?b ?c ~h n = 
    let c = match c with 
    | None -> min 0.88 (0.34 *. (float n) *. 0.06) 
    | Some c -> c 
    in
    (ignore (c); failwith "TODO")

  (* Diverging color schemes *)

  let div ?(w = 0.) ?(s = 0.6) ?(b = 0.75) ?(c = 0.88) ?(m = 0.5) ~h0 ~h1 = 
    failwith "TODO"

  let div_d ?w ?s ?b ?c ?m ~h0 ~h1 n = 
    let c = match c with 
    | None -> min 0.88 (1.0 -. 0.06 *. (11. -. (float (n / 2 + 1))))
    | Some c -> c
    in
    (ignore (c); failwith "TODO")

  (* Qualitative color schemes *)

  let of_byte r g b = 
    let btf c = (float c) /. 255. in
    Color.v (btf r) (btf g) (btf b) 1.

  type qual_fixed = 
    [ `Brewer_accent_8 
    | `Brewer_dark2_8 
    | `Brewer_paired_12 
    | `Brewer_pastel1_8 
    | `Brewer_pastel2_8 
    | `Brewer_set1_9
    | `Brewer_set2_8 
    | `Brewer_set3_12 
    | `Wijffelaars_17 ]

  let qual_fixed_size = function 
  | `Brewer_accent_8 -> 8 
  | `Brewer_dark2_8 -> 8
  | `Brewer_paired_12 -> 12 
  | `Brewer_pastel1_8 -> 8
  | `Brewer_pastel2_8 -> 8
  | `Brewer_set1_9 -> 9
  | `Brewer_set2_8 -> 8
  | `Brewer_set3_12 -> 12
  | `Wijffelaars_17 -> 17

  let qual_fixed ?size q = 
    let qsize = qual_fixed_size q in
    let size = match size with 
    | None -> qsize 
    | Some s -> if s > qsize then invalid_arg (err_scheme_size qsize s); s
    in
    let scheme = failwith "TODO" in
    Array.sub scheme 0 size

  let qual_d ?(eps = 0.) ?(r = 0.) ?(s = 0.5) ?(b = 1.) ?(c = 0.5) n = 
    failwith "TODO"
end


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
