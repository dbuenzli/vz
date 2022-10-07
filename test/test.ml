(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vz

let str = Format.sprintf
let log f = Format.printf (f ^^ "@?")
let fail fmt =
  let fail _ = failwith (Format.flush_str_formatter ()) in
  Format.kfprintf fail Format.str_formatter fmt

let raises f v = try f v; fail "didn't raise" with _ -> ()
let eqf v v' =
  if v = v' then () else
  fail "%f (%a) = %f (%a)" v Float.pp v v' Float.pp v'

let test_nice () =
  log "Testing nice numbers.\n";
  raises (Nice.step 0 1.) 10.;
  eqf (Nice.step 1 1. 1.) 0.;
  eqf (Nice.step 2 1. 1.) 0.;
  raises (Nice.step_floor ~step:(-0.1)) 2.;
  eqf  (Nice.step_floor ~step:2. 4.5) 4.;
  eqf  (Nice.step_floor ~step:2. 2.5) 2.;
  eqf  (Nice.step_floor ~step:2. 2.) 2.;
  eqf  (Nice.step_floor ~step:2. 1.5) 0.;
  eqf  (Nice.step_floor ~step:2. 0.) 0.;
  eqf  (Nice.step_floor ~step:2. (-0.5)) (-2.);
  eqf  (Nice.step_floor ~step:2. (-1.5)) (-2.);
  eqf  (Nice.step_floor ~step:2. (-2.5)) (-4.);
  raises (Nice.step_ceil ~step:(-0.1)) 2.;
  eqf  (Nice.step_ceil ~step:2. 4.5) 6.;
  eqf  (Nice.step_ceil ~step:2. 2.5) 4.;
  eqf  (Nice.step_ceil ~step:2. 2.) 2.;
  eqf  (Nice.step_ceil ~step:2. 1.5) 2.;
  eqf  (Nice.step_ceil ~step:2. 0.) 0.;
  eqf  (Nice.step_ceil ~step:2. (-0.5)) 0.;
  eqf  (Nice.step_ceil ~step:2. (-1.5)) 0.;
  eqf  (Nice.step_ceil ~step:2. (-2.5)) (-2.);
  eqf  (Nice.step_ceil ~step:2. (-3.5)) (-2.);
  eqf  (Nice.step_ceil ~step:2. (-4.5)) (-4.);
  raises (Nice.step_round ~step:(-0.1)) 2.;
  eqf  (Nice.step_round ~step:2. 5.5) 6.;
  eqf  (Nice.step_round ~step:2. 5.0) 6.;
  eqf  (Nice.step_round ~step:2. 4.9) 4.;
  eqf  (Nice.step_round ~step:2. 4.5) 4.;
  eqf  (Nice.step_round ~step:2. 2.5) 2.;
  eqf  (Nice.step_round ~step:2. 2.) 2.;
  eqf  (Nice.step_round ~step:2. 1.) 2.;
  eqf  (Nice.step_round ~step:2. 0.9) 0.;
  eqf  (Nice.step_round ~step:2. 0.) 0.;
  eqf  (Nice.step_round ~step:2. (-0.5)) 0.;
  eqf  (Nice.step_round ~step:2. (-1.0)) (-2.);
  eqf  (Nice.step_round ~step:2. (-1.1)) (-2.);
  eqf  (Nice.step_round ~step:2. (-2.5)) (-2.);
  eqf  (Nice.step_round ~step:2. (-3.0)) (-4.);
  eqf  (Nice.step_round ~step:2. (-3.5)) (-4.);
  eqf  (Nice.step_round ~step:2. (-4.5)) (-4.);
  let str_list acc prec v = (Printf.sprintf "%.*f" prec v :: acc) in
  raises (Nice.step_fold ~step:0. str_list [] (-0.1)) (3.);
  let result = ["0.3"; "0.2"; "0.1"; "0.0"; "-0.1"] in
  assert (Nice.step_fold ~step:0.1 str_list [] (-0.1) 0.3 = result);
  assert (Nice.step_fold ~step:0.1 str_list [] 0.3 (-0.1) = List.rev result);
  assert (Nice.step_fold ~step:0.1 str_list [] 0.32 (-0.11) = List.rev result);
  assert (Nice.step_fold ~step:0.1 str_list [] (-0.11) 0.32 = result);
  assert (Nice.step_fold ~step:0.1 str_list [] (-0.11) 0.39 = result);
  assert (Nice.step_fold ~step:0.1 str_list [] (-0.3) (-0.3) = [ "-0.3"]);
  assert (Nice.step_fold ~step:0.1 str_list [] (-0.31) (-0.31) = []);
  assert (Nice.step_outset ~step:1. (-1.11) (2.22) = (-2., 3.));
  assert (Nice.step_outset ~step:1. (2.22) (-1.11) = (3., -2.));
  assert (Nice.step_inset ~step:1. (-1.11) (2.22) = (-1., 2.));
  assert (Nice.step_inset ~step:1. (2.22) (-1.11) = (2., -1.));
  ()

(* Scales *)

let test_scale_linear () =
  log "Testing linear scales.\n";
  let pair set = match set with
  | `Continuous [x0; xn] -> (x0, xn)
  |  _ -> assert false
  in
  let s = Scale'.linear ~nice:true (-1.1, 2.1) (0., 1.) in
  let sf = Scale'.map s in
  let (rx0, rxn) = pair (Scale'.dom_unniced s) in
  let (x0, xn) = pair (Scale'.dom s) in
  let (y0, yn) = pair (Scale'.range s) in
  assert (rx0 = -1.1 && rxn = 2.1);
  assert (x0 = -2. && xn = 3.);
  assert (y0 = 0. && yn = 1.);
  assert (Scale'.niced s);
  assert (not (Scale'.clamp s));
  assert (sf (-2.) = 0.);
  assert (sf 3. = 1.);
  let s = Scale'.linear ~nice:true (2.1, -1.1) (0., 1.) in
  let sf = Scale'.map s in
  let (rx0, rxn) = pair (Scale'.dom_unniced s) in
  let (x0, xn) = pair (Scale'.dom s) in
  let (y0, yn) = pair (Scale'.range s) in
  assert (rx0 = 2.1 && rxn = -1.1);
  assert (x0 = 3. && xn = -2.);
  assert (y0 = 0. && yn = 1.);
  assert (Scale'.niced s);
  assert (not (Scale'.clamp s));
  assert (sf 3. = 0.);
  assert (sf (-2.) = 1.);
  ()

let test () =
  Printexc.record_backtrace true;
  test_nice ();
  test_scale_linear ();
  log "All tests succeded.\n"

let () = if not !Sys.interactive then test ()

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
