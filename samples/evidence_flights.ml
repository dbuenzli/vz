(*---------------------------------------------------------------------------
   Copyright (c) 2022 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Evidence

type obs =
  int * int * int * int * int * int * int * int * int * string *
  int * string * string * string * int * int * int * int * string

let t a b c d e f g h i j k l m n o p q s t =
  (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,s,t)

let p00 = fun (p,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> p
let p01 = fun (_,p,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> p
let p02 = fun (_,_,p,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> p
let p03 = fun (_,_,_,p,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> p
let p04 = fun (_,_,_,_,p,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> p
let p05 = fun (_,_,_,_,_,p,_,_,_,_,_,_,_,_,_,_,_,_,_) -> p
let p06 = fun (_,_,_,_,_,_,p,_,_,_,_,_,_,_,_,_,_,_,_) -> p
let p07 = fun (_,_,_,_,_,_,_,p,_,_,_,_,_,_,_,_,_,_,_) -> p
let p08 = fun (_,_,_,_,_,_,_,_,p,_,_,_,_,_,_,_,_,_,_) -> p
let p09 = fun (_,_,_,_,_,_,_,_,_,p,_,_,_,_,_,_,_,_,_) -> p
let p10 = fun (_,_,_,_,_,_,_,_,_,_,p,_,_,_,_,_,_,_,_) -> p
let p11 = fun (_,_,_,_,_,_,_,_,_,_,_,p,_,_,_,_,_,_,_) -> p
let p12 = fun (_,_,_,_,_,_,_,_,_,_,_,_,p,_,_,_,_,_,_) -> p
let p13 = fun (_,_,_,_,_,_,_,_,_,_,_,_,_,p,_,_,_,_,_) -> p
let p14 = fun (_,_,_,_,_,_,_,_,_,_,_,_,_,_,p,_,_,_,_) -> p
let p15 = fun (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,p,_,_,_) -> p
let p16 = fun (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,p,_,_) -> p
let p17 = fun (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,p,_) -> p
let p18 = fun (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,p) -> p

let year = Var.int "year" p00
let month = Var.int "month" p01
let day = Var.int "day" p02
let dep_time = Var.int "dep_time" p03
let sched_dep_time = Var.int "sched_dep_time" p04
let dep_delay = Var.int "dep_delay" p05
let arr_time = Var.int "arr_time" p06
let sched_arr_time = Var.int "sched_arr_time" p07
let arr_delay = Var.int "arr_delay" p08
let carrier = Var.nominal "carrier" p09
let flight = Var.int "flight" p10
let tailnum = Var.nominal "tailnum" p11
let origin = Var.nominal "origin" p12
let dest = Var.nominal "dest" p13
let air_time = Var.int "air_time" p14
let distance = Var.int "distance" p15
let hour = Var.int "hour" p16
let minute = Var.int "minute" p17
let time_hour = Var.nominal "time_hour" p18

let prod =
  Var.Prod.(unit t *
            year *
            month *
            day *
            dep_time *
            sched_dep_time *
            dep_delay *
            arr_time *
            sched_arr_time *
            arr_delay *
            carrier *
            flight *
            tailnum *
            origin *
            dest *
            air_time *
            distance *
            hour *
            minute *
            time_hour)

let doc = "tidyverse nyflights13 dataset"
let obs = Obs.v ~doc prod

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
