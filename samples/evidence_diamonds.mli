(*---------------------------------------------------------------------------
   Copyright (c) 2022 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Diamonds dataset.

    This is the {{:https://ggplot2.tidyverse.org/reference/diamonds.html}
    diamonds} dataset distributed with [ggplot2]. *)

open Evidence

type obs =
  float * string * string * string * float * float * int * float * float * float
(** The type for diamond observations. The 10 variables are as follows
    in order. *)

val carat : (obs, float) Var.t
(** [carat] is the diamond weight. *)

val cut : (obs, string) Var.t
(** [cut] is the cut quality, one of ["Fair"], ["Good"], ["Very Good"],
    ["Premium"], ["Ideal"]. *)

val color : (obs, string) Var.t
(** [color] is the diamond color from ["D"] (best) to ["J"] (worst). *)

val clarity : (obs, string) Var.t
(** [clarity] is a measurement of how clear the diamond is, one of
    (["I1"] (worst), ["SI2"], ["SI1"], ["VS2"], ["VS1"], ["VVS2"], ["VVS1"],
    ["IF"] (best). *)

val depth : (obs, float) Var.t
(** [depth] is the total depth percentage z / mean(x, y) = 2 * z / (x + y). *)

val table : (obs, float) Var.t
(** [table] is the width of top of diamond relative to widest point. *)

val price : (obs, int) Var.t
(** [price] is the diamond price in USD. *)

val x : (obs, float) Var.t
(** [x] is the diamond length in mm. *)

val y : (obs, float) Var.t
(** [y] is the diamond width in mm. *)

val z : (obs, float) Var.t
(** [y] is the diamond depth in mm. *)

val dataset : obs Dataset.t
(** [dataset] is a list of 53940 diamond observations. *)

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
