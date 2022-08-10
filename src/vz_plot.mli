(*---------------------------------------------------------------------------
   Copyright (c) 2022 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Quick cartesian plots of datasets.

    An {!Evidence} observation is represented by a graphical mark
    whose properties like position, color, opacity, etc. is defined by
    the values of its variables.

    {b Visual units.} To make it easier to think about the
    visualization in terms of human factors we assume the coordinate
    system of the visualization is in millimiters. But this being
    vector graphics the final rendering size depends on the size of
    the renderable.

    {b References.} The good ideas are from
    {{:http://dx.doi.org/10.1198/jcgs.2009.07098}ggplot2},
    {{:https://doi.org/10.1109/TVCG.2016.2599030}Vega-Lite},
    and {{:https://github.com/observablehq/plot}observable plot}. *)

open Gg
open Vz
open Evidence

(** Visual maps.

    Visual map indicate how to interpert and map variables
    to visual traits (coordinates, colors, symbols etc.) *)
module Vmap : sig

  type kind = [ `Qual | `Quant ]
  (** The type for how the variable should be interpreted. Note
      that variables can be interpreted both in a qualitative
      and quantiative manner in the same plot. *)

  type ('o, 'a) dom =
  | Dom of 'a Dom.t
  | Dom_deriver of
      (kind ->
       'o Dataset.t -> ('o, 'a) Var.t -> 'a Dom.Shape.t -> 'a Dom.t) (** *)
  (** The type for map domains. *)

  type 'a range =
  | Range of 'a Dom.t
  | Range_deriver of (kind -> w:float -> 'a Dom.t -> 'a Dom.t) (** *)
  (** The type for map ranges. *)

  type ('o, 'a) t
  (** The type for encoding a variable ['a] of observation of type [o] to
      graphical trait of type ['b]. *)

  val v :
    ?shape:'a Dom.Shape.t ->
    kind -> ('o, 'a) Var.t -> ('o, 'a) dom -> 'b range -> ('o, 'b) t

  val mag :
    ?shape:float Dom.Shape.t -> ?dom:float Dom.t -> ?range:float Dom.t ->
    kind -> ('o, float) Var.t -> ('o, float) t

(*  val area :
    ?shape:'a Dom.Shape.t -> ?dom:'a Dom.t -> ?range:float Dom.t ->
    kind -> ('o, 'a) Var.t -> ('o, float) t *)

  val color :
    ?shape:float Dom.Shape.t -> ?dom:float Dom.t -> ?range:color Dom.t ->
    kind -> ('o, float) Var.t -> ('o, Color.t) t

  val const : 'a -> ('o, 'a) t
end

(** Graphical representations of observations. *)
module Marks : sig

  type 'a t
  (** The type for marks representing a subset of observation of type
      ['a]. *)

  val dot :
    ?r:('o, float) Vmap.t ->
    ?x:('o, float) Vmap.t ->
    ?y:('o, float) Vmap.t ->
    ?fill:('o, Color.t) Vmap.t ->
    ?outline:('o, Color.t) Vmap.t ->
    unit -> 'o t

  val to_image : 'a t -> w:float -> 'a Dataset.t -> Box2.t * Vg.image
end


(** Plots *)
module Plot : sig
  type t
  (** The type for cartesian plots. *)
  val empty : t
  val v : ?w:float -> ?h:float -> ('a Marks.t * 'a Dataset.t) list -> t
  (** [v ?w ?h ()] is a plot with given attributes:
      {ul
      {- [w] is the plot width including margins. If unspecified this is 160mm.}
      {- [h] is the plot's height. If unspecified this is determined by
         the plot's vertical scales.}} *)

  val over : t -> t -> t
  val to_image : t -> Box2.t * Vg.image
  (** [to_image t] is an image for the plot and its extents (including
      margins). The plot's lower-left corner (including margin) is at
      the origin. *)
end

(*
(** Marks.

    Marks represent values graphically. A mark is made of
    a number of channels, like it's position or color which
    are defined in terms of the value's projections. *)
module Mark : sig

  (*
  type kind =
  [ `Bar | `Circle | `Square | `Tick | `Line | `Area | `Point | `Rule
  | `Text | `Image of Vg.image ]
  (** The type for mark kinds. *)
*)

  type 'a kind
  (** The type for mark kinds. *)

  module Dot : sig
    val v :
      ?shape:('a, Vg.image) Channel.t ->
      ?size:('a, float) Channel.t -> unit -> 'a kind
  end

  type 'a validator = 'a -> 'a option
  (** The type for validating values. *)

  type 'a t
  (** The type for marks representing rows of ['a]. *)

  (** XX combinators to alter opacity/width *)
  val v :
    ?descr:string -> ?class':string -> ?hoverbox:'a Hoverbox.t ->
    ?area:('a, Vg.image) Channel.t ->
    ?outline:('a, Vg.image) Channel.t ->
    ?opacity:('a, float) Channel.t ->
    ?x:('a, float) Channel.t ->
    ?y:('a, float) Channel.t ->
    'a kind -> 'a t
  (** [v kind] is a mark of kind [kind] and given parameters.
      See corresponding accessors for semantics and defaults. *)
end

*)

(*
(** Column descriptions.

    Column values describe row columns. *)
module Col : sig

  type name = string
  (** The type for column names. *)

  type 'a kind =
  | Int : int kind
  | Float : float kind
  | Bool : bool kind
  | Categorical : 'a kind
  | Ordinal : ('a -> int) -> 'a kind
  (** The type for kind of values in the column.

      For ordinality the function should return non-negative numbers
      starting from [0]. *)

  type ('r, 'a) t
  (** The type for data columns. *)

  type 'r v = V : ('r, 'a) t -> 'r v
  (** The type for existaential columns for a row of type ['r]. *)

  val v :
    ?na:'a -> ?pp:(Format.formatter -> 'a -> unit) ->
    ?cmp:('a -> 'a -> int) -> name -> 'a kind -> ('r -> 'a) -> ('r, 'a) t
  (** [v name proj] is a column with given attributes. See corresponding
      accessors for semantics and defaults. *)

  val name : ('r, 'a) t -> string
  (** [name c] is the name of [c]. *)

  val kind : ('r, 'a) t -> 'a kind
  (** [kind c] is the kind of [c]. *)

  val proj : ('r, 'a) t -> 'r -> 'a
  (** [proj c] is the projection function of [c]. *)

  val value_compare : ('r, 'a) t -> ('a -> 'a -> int)
  val value_pp : ('r, 'a) t -> (Format.formatter -> 'a -> unit)

  (** {1:convenience Convenience constructors} *)

  val int : name -> ('r -> int) -> ('r, int) t
  val float : name -> ('r -> float) -> ('r, float) t
end

(** Row descriptions. *)
module Row : sig

  type 'a t
  (** The type for rows. *)

  val v : 'a Col.v list -> 'a t
end


module Hoverbox : sig
  type 'a t
end

module Scale : sig
end

(** Channels.

    Channel define data sources from values by transforming them
    via a scale.

    {b XXX} Map ? *)
module Channel : sig

  type ('a, 'b) t
  (** The type for channels. *)

  val of_col : ('a, 'b) Col.t -> ('a, 'b) t

  val const : 'b -> ('a, 'b) t
end




module View : sig
  type t
  val empty : t
  val of_plot : Plot.t -> t
  val vcat : ?gutter_mm:float -> t -> t -> t
  val hcat : ?gutter_mm:float -> t -> t -> t
(* Maybe we don't need repeat but there's something
   about scale and merging *)

end
*)



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
