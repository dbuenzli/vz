(*---------------------------------------------------------------------------
   Copyright (c) 2022 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Brr
open Note
open Gg
open Vg
open Vz_doc

let strf = Format.asprintf

;; Doc.md {|
# A sample document

We begin by determining the GCD we then proceed.
|}

let () = Doc.el (El.h1 [El.txt' "A sample document"])
let e = Input.trigger () (`Txt "Click me!") |> Doc.input
let e = Input.trigger () ~label:(`Txt "Label") (`Txt "Click me!") |> Doc.input

let count =
  let reset = Fun.const 0, `Txt "Reset" in
  let decr = Int.pred, `Txt "Decr" in
  let incr = Int.succ, `Txt "Incr" in
  let ops = [reset; decr; incr] in
  Input.triggers ~label:(`Txt "Counter:") ~init:0 ops |> Doc.input

let () = Doc.txt' (S.map (fun c -> strf "The counter value is %d." c) count)

let b = Input.bool ~init:true (`Txt "Check me!") |> Doc.input
let () = Doc.txt' (S.map (fun b -> strf "The checkbox is %b" b) b)
let f = Input.float 0.5 ~label:(`Txt "Slide me!") |> Doc.input
let () = Doc.txt' (S.map (fun f -> strf "The slider is %g" f) f)

let file = Input.file (`Txt "Load file…") |> Doc.input
let file = S.hold None (E.Option.some file)
let show_filename f =
  let p = File.relative_path f in
  Jstr.to_string (if Jstr.is_empty p then p else File.name f)


let filename = S.map (Option.map show_filename) file
let () =
  Doc.txt'
    (S.map (fun f -> strf "The file is %s" (Option.value ~default:"none" f))
       filename)

let accept = [".ml"; ".md"]
let files = Input.files ~accept ~select:`Dir (`Txt "Load files…") |> Doc.input
let files = S.hold [] files
let () =
  Doc.txt'
    (S.map (fun files -> strf "The files are %s"
               (String.concat "\n" (List.map show_filename files))) files)


let circle r =
  let circle = P.empty |> P.circle (P2.v 0.5 0.5) r in
  I.cut circle (I.const (Color.gray 0.5))

let () =
  Output.image
    ~size_mm:(V2.v 10. 10.) ~view:Box2.unit (circle 0.4) |> Doc.output

let () =
  Output.image'
    ~size_mm:(V2.v 10. 10.)
    ~view:(S.const Box2.unit) (S.map circle f) |> Doc.output


;; Doc.md {|
In this section we try to initiate the rendering along the following points:

- Do not go to bed too early
- Make sure all the lights are off
- Self-deprecate |}

let () = Doc.render ()

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
