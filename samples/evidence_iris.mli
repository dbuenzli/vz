(*---------------------------------------------------------------------------
   Copyright (c) 2022 The evidence programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Iris dataset.

    This is the Fisher {{:http://archive.ics.uci.edu/ml/datasets/Iris}Iris
    dataset}. *)

open Evidence

type obs = float * float * float * float * string
(** The type for iris observations. The 5 variables are as follows, in order. *)

val sepal_length : (obs, float) Var.t
(** [sepal_length] is the sepal length in cm. *)

val sepal_width : (obs, float) Var.t
(** [sepal_width] is the sepal width in cm. *)

val petal_length : (obs, float) Var.t
(** [petal_length] is the petal length in cm. *)

val petal_width : (obs, float) Var.t
(** [petal_width] is the petal width in cm. *)

val species : (obs, string) Var.t
(** [species] is the species, one of ["setosa"], ["versicolor"],
    ["virignica"]. *)

val obs : obs Obs.t
(** [obs] describes an iris observation. *)

val dataset : obs Dataset.t
(** [dataset] is a list of 150 iris observations. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The evidence programmers

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
