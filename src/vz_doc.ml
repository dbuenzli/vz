(*---------------------------------------------------------------------------
   Copyright (c) 2022 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Note
open Brr
open Brr_note

module Input = struct
  type t = El.t
  type content = [ `Txt of string | `Els of El.t list signal ]

  let el = Fun.id
  let vz_class = Jstr.v "vz-input"

  let set_content el = function
  | `Txt "" -> ()
  | `Txt txt -> El.append_children el [El.txt' txt]
  | `Els els -> Elr.def_children el els

  let set_enabled ?(enabled = S.Bool.true')  el =
    let disabled_of enabled = if enabled then None else Some Jstr.empty in
    Elr.def_at At.Name.disabled (S.map disabled_of enabled) el

  let make ?label inputs =
    let cs = match label with
    | None -> inputs
    | Some `Txt "" -> inputs
    | Some content ->
        let label = El.label [] in
        set_content label content;
        label :: inputs
    in
    El.div ~at:[At.class' vz_class] cs

  let make_button ?enabled content on_click =
    let button = El.button [] in
    let act = Evr.on_el Ev.click on_click button in
    set_content button content;
    set_enabled button ?enabled;
    act, button

  (* Triggers *)

  let trigger ?label ?enabled v content =
    let act, button = make_button ?enabled content (Evr.stamp v) in
    act, make ?label [button]

  let triggers ?label ?enabled ~init triggers =
    let but (f, content) = make_button ?enabled content (Evr.stamp f) in
    let evs, buttons = List.split (List.map but triggers) in
    let s = S.hold init (E.accum init (E.select evs)) in
    s, make ?label buttons

  (* Booleans *)

  let make_checkbox ?enabled ~init () =
    let i = El.input ~at:At.[type' (Jstr.v "checkbox"); if' init checked] () in
    let act = Evr.on_el Ev.change (fun _ -> El.prop El.Prop.checked i) i in
    let s = S.hold init act in
    set_enabled i ?enabled;
    s, i

  let bool ?label ?enabled ~init content =
    let s, c = make_checkbox ?enabled ~init () in
    let c = match content with
    | `Txt "" -> El.label [c]
    | `Txt txt -> El.label [c; El.txt' txt]
    | `Els els ->
        let label = El.label [] in
        Elr.def_children label (S.map (List.cons c) els);
        label
    in
    s, make ?label [c]

  (* Numbers *)

  type number_affordance = [ `Slider | `Text_and_slider | `Text ]

  (* FIXME factor out. *)

  let make_int_slider ?enabled ?min ?max ?step init =
    let min = Option.value ~default:(S.const 1) min in
    let max = Option.value ~default:(S.const 100) max in
    let type' = At.type' (Jstr.v "range") in
    let i = El.input ~at:[type'] () in
    let some_int s = Some (Jstr.of_int s) in
    let () = Elr.def_at (Jstr.v "min") (S.map some_int min) i in
    let () = Elr.def_at (Jstr.v "max") (S.map some_int max) i in
    let () = match step with
    | None -> El.set_at (Jstr.v "step") (Some (Jstr.v "1")) i
    | Some s -> Elr.def_at (Jstr.v "step") (S.map some_int s) i
    in
    let () = El.set_at At.Name.value (Some (Jstr.of_int init)) i in
    let get_value _ =
      Option.value ~default:0 (Jstr.to_int (El.prop El.Prop.value i))
    in
    let act = Evr.on_el Ev.input get_value i in
    let s = S.hold init act in
    set_enabled i ?enabled;
    s, i

  let make_float_slider ?enabled ?min ?max ?step init =
    let min = Option.value ~default:(S.const 0.) min in
    let max = Option.value ~default:(S.const 1.) max in
    let type' = At.type' (Jstr.v "range") in
    let i = El.input ~at:[type'] () in
    let some_float s = Some (Jstr.of_float s) in
    let () = Elr.def_at (Jstr.v "min") (S.map some_float min) i in
    let () = Elr.def_at (Jstr.v "max") (S.map some_float max) i in
    let () = match step with
    | None -> El.set_at (Jstr.v "step") (Some (Jstr.v "any")) i
    | Some s -> Elr.def_at (Jstr.v "step") (S.map some_float s) i
    in
    let () = El.set_at At.Name.value (Some (Jstr.of_float init)) i in
    let get_value _ = Jstr.to_float (El.prop El.Prop.value i) in
    let act = Evr.on_el Ev.input get_value i in
    let s = S.hold init act in
    set_enabled i ?enabled;
    s, i

  let int ?label ?enabled ?affordance ?min ?max ?step init =
    let s, i = make_int_slider ?enabled ?min ?max ?step init in
    s, make ?label [i]

  let float ?label ?enabled ?affordance ?min ?max ?step init =
    let s, i = make_float_slider ?enabled ?min ?max ?step init in
    s, make ?label [i]

  (* Enumerations *)

  type enum_affordance = [ `Buttons | `Menu ]

  let one_of ?label ?enabled ?affordance ?(eq = ( = )) enum ~init names =
    failwith "TODO"

  let list ?label ?enabled ?affordance ?eq enum ~init names =
    failwith "TODO"

  (* Files *)

  type file_type = string

  let make_file_input ?enabled ?(accept = []) mode content get =
  (* The label of file inputs can't be set… so we use
     a button that forwards its click to a hidden input element. *)
    let i =
      let accept = match accept with
      | [] -> At.void
      | types -> At.v (Jstr.v "accept") (Jstr.v (String.concat "," types))
      in
      let multiple = match mode with
      | `Files | `Dirs -> At.true' (Jstr.v "multiple") | _ -> At.void
      in
      let wdir = match mode with
      | `Dir | `Dirs -> At.true' (Jstr.v "webkitdirectory") | _ -> At.void
      in
      El.input ~at:At.[type' (Jstr.v "file"); multiple; wdir; accept] ()
    in
    let button = El.button [] in
    let forward _ =
      (* If the same file gets selected events do not refire,
         resetting the value property here works around this problem. *)
      El.set_prop El.Prop.value Jstr.empty i; El.click i
    in
    let () = ignore (Ev.listen Ev.click forward (El.as_target button)) in
    let act = Evr.on_el Ev.change (fun _ -> get i) i in
    El.set_inline_style El.Style.display (Jstr.v "none") i;
    set_content button content;
    set_enabled button ?enabled;
    act, [button; i]

  let file ?label ?enabled ?accept name =
    let get i = List.hd (El.Input.files i) in
    let act, i = make_file_input ?enabled ?accept `File name get in
    act, make ?label i

  type files_select = [ `Files | `Dir | `Dirs ]

  let files ?label ?enabled ?accept ~select name =
    let get i = El.Input.files i in
    let act, i = make_file_input ?enabled ?accept select name get in
    act, make ?label i

end

module Output = struct
  open Gg
  open Brr_canvas

  type t = El.t
  let el = Fun.id
  let vz_class = Jstr.v "vz-output"

  let make ?label outputs =
    (* FIXME we should make a caption. *)
    let cs = match label with
    | None -> outputs
    | Some `Txt "" -> outputs
    | Some content ->
        let label = El.label [] in
        Input.set_content label content;
        label :: outputs
    in
    El.div ~at:[At.class' vz_class] cs

  let image ?label ~size_mm ?(view = Box2.v P2.o size_mm) i =
    let c = Brr_canvas.Canvas.create [] in
    let r = Vg.Vgr.create (Vgr_htmlc.target c) `Other in
    let () = ignore (Vg.Vgr.render r (`Image (size_mm, view, i))) in
    let () = ignore (Vg.Vgr.render r `End) in
    make ?label [Brr_canvas.Canvas.to_el c]

  let image'
      ?label ~size_mm ?(view = S.const (Box2.v P2.o size_mm)) i
    =
    let c = Brr_canvas.Canvas.create [] in
    let r = Vg.Vgr.create (Vgr_htmlc.target c) `Other in
    let render view i =
      ignore (Vg.Vgr.render r (`Image (size_mm, view, i))) in
    let log = Logr.(const render $ S.obs view $ S.obs i) in
    Logr.hold (Logr.create log);
    make ?label [Brr_canvas.Canvas.to_el c]
end

module Doc = struct
  let rev_doc = ref []
  let add el = rev_doc := el :: !rev_doc

  let input (v, i) = add i; v
  let output o = add o
  let md s = add (El.p [El.txt' s])
  let el el = add el

  let txt fmt = Format.kasprintf (fun s -> add (El.p [El.txt' s])) fmt
  let txt' txt =
    let p = El.p [] in
    let () = Elr.def_children p (S.map (fun t -> [El.txt' t]) txt) in
    add p

  let css s = add (El.style [El.txt' s])

  let rec render ?css:(add_css = true) ?root () =
    if add_css then css default_css;
    let root = match root with
    | None -> Document.body (Brr.G.document)
    | Some el -> el
    in
    El.append_children root (List.rev !rev_doc);
    rev_doc := []

  and default_css =
{css|
.vz-input + .vz-input { margin-top: 1ex }
.vz-input label + * { margin-left: 1ex }
|css}
end

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
