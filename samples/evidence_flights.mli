(*---------------------------------------------------------------------------
   Copyright (c) 2022 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Flights dataset.

    This the {{:https://nycflights13.tidyverse.org/}[nycflights13]}
    dataset.

    {b Note.} We don't provide the data it's too heavy. This is just
    the schema. (XXX provide a link to the CSV) *)

open Evidence

type obs =
  int * int * int * int * int * int * int * int * int * string *
  int * string * string * string * int * int * int * int * string
(** The type for flight observations. The 19 variables are as follows,
    in order.*)

val year : (obs, int) Var.t
val month : (obs, int) Var.t
val day : (obs, int) Var.t
val dep_time : (obs, int) Var.t
val sched_dep_time : (obs, int) Var.t
val dep_delay : (obs, int) Var.t
val arr_time : (obs, int) Var.t
val sched_arr_time : (obs, int) Var.t
val arr_delay : (obs, int) Var.t
val carrier : (obs, string) Var.t
val flight : (obs, int) Var.t
val tailnum : (obs, string) Var.t
val origin : (obs, string) Var.t
val dest : (obs, string) Var.t
val air_time : (obs, int) Var.t
val distance : (obs, int) Var.t
val hour : (obs, int) Var.t
val minute : (obs, int) Var.t
val time_hour : (obs, string) Var.t

val obs : obs Obs.t
(** [obs] describes a flight observation. *)

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
