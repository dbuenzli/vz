(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Declarative data visualization with [Vg].

    [Vz] provides tools to map data to [Vg] images.

    This is a low level layer. For quick exploratory visualizations
    use {!Vz_plot}.

    Open the module to use it, this defines only modules. *)

open Gg
open Vg

(** Nicing numbers.

    {b XXX} Maybe move that to [Gg.Float]

    Nice numbers for nice labels and ticks. *)
module Nice : sig

  (** {1:steps Nice steps} *)

  val step : int -> float -> float -> float
  (** [step n v0 v1] is a nice step size for sampling the directed
      interval \[[v0];[v1]\] {e approximatively} [n] times. The step
      size is in the set \{ q⋅10{^z} | q ∈ \{1,2,5\} and z ∈ Z \} or
      [0] if [v0 = v1]. Raises [Invalid_argument] if [n <= 0]. *)

  (** {1:quantize Quantized nicing}

      Given a positive step size [step] the following functions nice
      numbers so that they belong to the set Q([step]) = \{ z⋅[step] |
      z ∈ Z \}. *)

  val step_floor : step:float -> float -> float
  (** [step_floor step v] is the greatest [v'] such
      that [v' <= v] and [v'] ∈ Q([step]). Raises [Invalid_argument]
      if [step <= 0.]. *)

  val step_ceil : step:float -> float -> float
  (** [step_ceil step v] is the smallest [v'] such
      that [v <= v'] and [v'] ∈ Q([step]). Raises [Invalid_argument]
      if [step <= 0.]. *)

  val step_round : step:float -> float -> float
  (** [step_round step v] is either [step_floor v] or [ceil_floor v]
      whichever is the nearest. Ties are rounded towards away from zero.
      Raises [Invalid_argument] if [step <= 0.]. *)

  val step_fold :
    step:float -> ('a -> int -> float -> 'a) -> 'a -> float -> float -> 'a
  (** [step_fold step f acc v0 v1] folds [f] over set of numbers
      Q([step]) ∩ \[[v0];[v1]\]. Folding is performed in order and in
      the direction from [v0] to [v1]. The integer given to [f]
      is the fractional precision of the numbers mandated by [step].
      Raises [Invalid_argument] if [step <= 0.]. *)

  val step_outset : step:float -> float -> float -> (float * float)
  (** [step_outset step v0 v1] {e expands} the directed
      interval \[[v0];[v1]\] to [(v0',v1')] ∈ Q([step]){^2} where
      [v0'] (resp. [v1']) is the nearest number to [v0] (resp. [v1]).
      Raises [Invalid_argument] if [step <= 0.]. *)

  val step_inset : step:float -> float -> float -> (float * float)
  (** [step_inset step v0 v1] {e shrinks} the directed
      interval \[[v0];[v1]\] to [(v0',v1')] ∈ Q([step]){^2} where
      [v0'] (resp. [v1']) is the nearest number to [v0] (resp. [v1]).
      Raises [Invalid_argument] if [step <= 0.]. *)
end

(** Domains.

    Domains describe sets of continuous or discrete values and how to
    inject them on the unit interval \[[0]; [1.]\] and project them back.

    Domains are also always equipped with a comparison function, a
    formatter and a function to select values for tick marks. *)
module Dom : sig

  type 'a compare = 'a -> 'a -> int
  (** The type for comparisons functions. *)

  type 'a fmt = Format.formatter -> 'a -> unit
  (** The type for formatting functions. *)

  type piecewise_proj =
  | Proportional
  (** The bounds of the pieces are proportionally projected on the unit space.
      For example the bounds of the pieces [[0.; 8.; 10.]] are mapped on
      [[0.; 0.8; 1.]]. *)
  | Uniform
  (** The bounds of the pieces are uniformly projected on the unit space.
      For example [[0.; 2.; 10.]] are mapped on [[0.; 0.5; 1.]]. *)
  (** The type for projections on the unit interval for piecewise
      continuous domains. *)

  type kind =
  | Discrete (** Finite and ordered list of values. *)
  | Continuous of piecewise_proj (** Piecewise continuous domain. *)
  (** The kind of domain, either discrete or continuous. *)

  (** Domain shaping. *)
  module Shape : sig
    type 'a t
    val v : ('a -> 'a) -> inv:('a -> 'a) -> 'a t
    val linear : float t
    val log : base:float -> float t
    val pow : exp:float -> float t
    val sqrt : float t
  end

  type 'a t
  (** The type for domains of type ['a]. *)

  val v :
    ?clamped:bool -> ?compare:'a compare ->
    ?proj_extents:(float -> 'a * 'a) -> ?unniced_values:'a list ->
    ?pp:'a fmt -> kind -> none:'a -> 'a list ->
    ('a -> float) -> (float -> 'a) -> 'a t

  val kind : 'a t -> kind
  (** [kind d] is the domain kind see {!type-kind}. *)

  val inj : 'a t -> ('a -> float)
  (** [inj d v] maps value [v] of the domain in the unit interval
      \[[0]; [1.]\]. If [clamped d] is [false] the resulting value can
      lie outisde the unit interval. If [v] cannot be mapped it maps
      to {!Float.nan}.  *)

  val proj : 'a t -> (float -> 'a)
  (** [inj d t] is the inverse of {!proj}, it maps [t] back to
      the domain value, if possible. If [clamped d] is [true] [t]
      is clamped to \[[0];[1.]\]. If [t] cannot be mapped back it maps
      to [none d]. *)

  val proj_extents : 'a t -> (float -> 'a * 'a)
  (** [inv_map d t] is the inverse of {!map}, but maps back to a range
      of the original domain. This is useful if [inv_map] is a
      quantization of the unit interval. If it's not this returns
      the point range [inv_map d t]. *)

  val values : 'a t -> 'a list
  (** [values d] is the list of values defining the domain. The
      semantics of which depends on {!kind}. *)

  val none : 'a t -> 'a
  (** [none d] is the value returned by invert map when the value
      cannot be mapped back. *)

  (** {1:continuous Continuous} *)

  val linear : ?nice:bool -> ?clamp:bool -> float -> float -> float t

  val log :
    ?base:float -> ?nice:bool -> ?clamp:bool -> float -> float -> float t

  val pow :
    ?exp:float -> ?nice:bool -> ?clamp:bool -> float -> float ->
    float t

  val sqrt : ?nice:bool -> ?clamp:bool -> float -> float -> float t

  (** {1:discrete Discrete} *)

  val discrete :
    ?compare:'a compare -> ?pp:'a fmt -> ?none:'a -> 'a list -> 'a t
  (** [discrete vs] is a discrete domain with values [vs]. Given [n]
      values the [i]th zero-based position in the order is injected to
      [i/n]. The projection maps any value in the interval \[[i/n];
      [(i+1)/n]\[ back to [i]. [1.] maps to [none] which by default
      is the last [n-1]th element. *)

end

(** Maps between domains. *)
module Dmap: sig
  type ('a, 'b) t
  (** The type for mapping domains of type ['a] to a domain of type ['b]. *)

  val v : 'a Dom.t -> 'b Dom.t -> ('a, 'b) t
  val dom : ('a, 'b) t -> 'a Dom.t
  val codom : ('a, 'b) t -> 'b Dom.t
  val map : ('a, 'b) t -> 'a -> 'b
  val inv_map : ('a, 'b) t -> 'b -> 'a
  val inv : ('a, 'b) t -> ('b, 'a) t
  val compose : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val merge : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
end

(** Scales.

    Scales map data values to visual values.

    A scale represents a function from a given domain to a given
    range. It keeps additional metadata about the map which can be
    used for example to represent them as axes in visual
    depictions. *)
module Scale' : sig

  type 'a set =
  [ `Discrete of 'a list
  | `Continuous of 'a list ]
  (** The type for representing sets of values. *)


  (** {1:scales Scales} *)

  type ('a, 'b) t
  (** The type for scales mapping values from a domain of type ['a] to
      a range of ['b]. *)

  val dom : ('a, 'b) t -> 'a set
  (** [dom s] is the (possibly niced) domain of [s]. *)

  val dom_unniced : ('a, 'b) t -> 'a set
  (** [dom_unniced s] is the unniced domain of [s]. This is the same
      as {!dom} if [niced] is false. *)

  val range : ('a, 'b) t -> 'b set
  (** [range s] is the range of [s]. *)

  val clamp : ('a, 'b) t -> bool
  (** [clamp s] is [true] if the map of [s] first clamps values to
      the scale domain before mapping them (ensures the map returns
      only values in the range). *)

  val niced : ('a, 'b) t -> bool
  (** [niced s] is [true] if the domain of [s] was niced. *)

  (** {1:maps Maps and invert map} *)

  val map : ('a, 'b) t -> ('a -> 'b)
  (** [map s] is the mapping function of [s].

      {b Warning.} On ordinal scales the mapping function raises
      [Invalid_argument] on undefined arguments. Use {!partial_map} to
      apply to unknown values. *)

  val partial_map : ('a, 'b) t -> ('a -> 'b option)
  (** [partial_map s] is like [map s] except on ordinal scales
      it returns [None] on undefined argument. *)

(*  val invert_map : ('a, 'b) t -> 'b -> 'a *)

  val fold_ticks : ?bounds:bool -> int -> ('a -> int -> float -> 'a) -> 'a ->
    (float, 'b) t -> 'a
  (** [fold_ticks bounds n f acc scale] folds [f] over {e approximatevely} [n]
      uniformly spaced values taken in [scale]'s domain using {!Nice.step}
      to determine the step value. See {!Nice.step_fold}. If [bounds]
      is [true] ensures that the bounds of the domain are folded over. *)

  (** {1:continuous Continuous scales} *)


  val linear : ?clamp:bool -> ?nice:bool ->
    (float * float) -> (float * float) -> (float, float) t
  (** [linear ?clamp ?nice (x0, x1) (y0, y1)] is the map that
      linearly transforms [x0] on [y0] and [x1] on [y1]. If
      the map is undefined ([x0 = x1] and [y0 <> y1]) the
      map always returns [y0].
      {ul
      {- If [clamp] is [false] (default), the scale maps values outside
         the domain [(x0, x1)] according to the specified linear
         transformation. If [true] values outside the domain are
         clamped to the nearest domain bounds.}
      {- If [nice] if [true] the given domain [(x0, x1)] is first {e expanded}
         to fall on round numbers [(x0', x1')] which are
         used to define the linear map. The precision of these round
         numbers is one order of magnitude less than the extent of the
         domain that is: 10{^(round
         (log{_10} abs_float (x1 - x0)) - 1)}. Defaults to [false].}} *)

(*
  val poly_linear_p : ?clamp:bool -> ?nice:bool -> float list -> float list ->
    (float, float) scale
  (** [linear_p ?clamp ?nice dom range] is like {maps numbers according to the
      piecewise linear function defined by dom and range (which must
      have the same length). See {!linear}. *)
*)

  (** {1 Ordinal scales}

      Ordinal scales maps a discrete orderable domain to a range. *)

  val ordinal : ?cmp:('a -> 'a -> int) -> 'a list -> 'b list -> ('a, 'b) t
  (** [ordinal cmp dom range] maps the value [dom]{_i} to the value
      [range]{_i mod max} with max [= List.length range - 1].
      [cmp] is the order on the domain, defaults to [Pervasives.compare]. *)

  (** {1 Generating discrete ranges} *)

  val range_pts : ?rpad:float -> min:float -> max:float -> int -> float list
  val range_bands : ?rpad:float -> ?pad:float -> min:float -> max:float ->
    int -> float list
end

(** {1:sum Statistics} *)

type ('a, 'b) stat
(** The type for a statistic of type ['b] on data values of type ['a]. *)

(** Data statistics.

    [Stat] summarizes data with statistics. *)
module Stat : sig

  (** {1:sum Statistics} *)

  type ('a, 'b) t = ('a, 'b) stat
  (** The type for a statistic of type ['b] on data of type ['a]. *)

  val add : ('a, 'b) stat -> 'a -> ('a, 'b) stat
  (** [add s v] is the statistic [s] with value [v] added to the data. *)

  val add_flip : 'a -> ('a, 'b) stat -> ('a, 'b) stat
  (** [add_flip v s] is [add s v]. *)

  val value : ('a, 'b) stat -> 'b
  (** [value s] is the value of statistic [s]. *)

  (** {1:prim Primitive statistics combinators} *)

  val count : ('a, float) stat
  (** [count s] is the {e integral} number of values in the data. *)

  val min : ('a -> float) -> ('a, float) stat
  (** [min f] is the minimum value of [f] on the data. *)

  val max : ('a -> float) -> ('a, float) stat
  (** [max f] is the maximum value of [f] on the data. *)

  val range : ('a -> float) -> ('a, float * float) stat
  (** [range f] is the range of [f] on the data, equivalent
      to [t2 (min f) (max f)]. *)

  val range_d : ?cmp:('b -> 'b -> int) -> ('a -> 'b) -> ('a, 'b list) stat
  (** [range_d cmp f] is the discrete range of [f], the {e set} of
      values returned by [f] on the data. [cmp] is used
      to compare the values (defaults to [Pervasives.compare]). *)

  val sum : ?nan:bool -> ('a -> float) -> ('a, float) stat
  (** [sum nan f] is the sum of the values returned by [f] on the
      data. If [nan] is [false] (default), [nan] values are
      ignored. *)

  val mean : ?nan:bool -> ('a -> float) -> ('a, float) stat
  (** [mean nan f] is the mean of the values returned by [f] on the
      data. If [nan] is [false] (default), [nan] values are ignored. *)

  val mean_var : ?nan:bool -> ?pop:bool -> ('a -> float) ->
    ('a, float * float) stat
  (** [mean_var nan pop f] is the mean and {e unbiased}
     {{:http://mathworld.wolfram.com/SampleVariance.html}sample
     variance} of the values returned by [f] on the sample data.  If
     [pop] is [true] (defaults to [false]), the population variance
     ({e biased} sample variance) is computed.  If [nan] is [false]
     (default), [nan] values are ignored.  *)

(* TODO

  val median : ?sorted:bool -> ?count:int -> ('a -> float) -> ('a, float) stat
  (** [median sorted f] is the median of the values of [f] on the data.
      If [sorted] is [true] (default to [false]) the values are assumed
      to be sorted.

      {b Warning}. This bufferizes the results of [f] if  *)

  val quantile : ?sorted:bool -> float -> ('a -> float) -> ('a, float) stat
  (** [quantile sort p f] is the [p]-quantile of [f] on the data.
      If [sort] is [false] (defaults) the values are assumed to be sorted. *)

*)

  val fold : ('b -> 'a -> 'b) -> 'b -> ('a, 'b) stat
  (** [fold f acc] is [f] folded on the the data starting with
      [acc]. *)

  (** {1 Higher-order statistic combinators} *)

  val list : ('a, 'b) stat list -> ('a, 'b list) t
  (** [list l] is the combined statistics of [l] on the data. *)

  val t2 : ('a , 'b) stat -> ('a, 'c) stat -> ('a, 'b * 'c) stat
  (** [t2 s1 s2] is the combined statistics of [s1] and [s2] on the data. *)

  val t3 : ('a , 'b) stat -> ('a, 'c) stat -> ('a, 'd) stat ->
    ('a, 'b * 'c * 'd) stat
  (** [t3 s1 s2 s3] is the combined statistics of [s1], [s2] and [s3]
      on the data. *)

  val t4 : ('a , 'b) stat -> ('a, 'c) stat -> ('a, 'd) stat -> ('a, 'e) stat ->
    ('a, 'b * 'c * 'd * 'e) stat
  (** [t4 s1 s2 s3 s4] is the combined statistics of [s1], [s2], [s3] and
      [s4] on the data. *)

  val t5 : ('a , 'b) stat -> ('a, 'c) stat -> ('a, 'd) stat -> ('a, 'e) stat ->
    ('a, 'f) stat -> ('a, 'b * 'c * 'd * 'e * 'f) stat
  (** [t5 s1 s2 s3 s4 s5] is the combined statistics of [s1], [s2], [s3], [s4]
      and [s5] on the data. *)
end



(** {1 Image and path helpers} *)


(** Marks.

    This module provides a few convenience combinators to create
    paths.
*)
module Mark : sig

  (** {1:align_mark Mark alignement} *)

  type halign = [ `Center | `Left | `Right ]
  (** The type for horizontal alignements. *)

  type valign = [ `Center | `Bottom | `Top ]
  (** The type for vertical alignements. *)

(*
  type orientation = [ `Left | `Top | `Right | `Bottom ]
  type hdir = [ `Left | `Right ]
  type vdir = [ `Up | `Down ]
*)

  (** {1:align Marks}

      Mark constructors share the following optional parameters.
      {ul
      {- [contour], if [true] ensures that the path defines a bounded
         area of the plane (defaults to [false]).}
      {- [normalize], if [true] adjusts the size of the mark
         so that it's area is equivalent to size of a square of the
         given width.}
      {- [path], the path to add the mark to (defaults to {!P.empty}.).
         Marks always start a new subpath.}
      {- [pos], the location of the mark (defaults to {!P2.o}).}
      {- [halign], mark horizontal alignement with respect to [pos] (defaults
         to [`Center]).}
      {- [valign], mark vertical alignement with respect to [pos]
      (defaults to [`Center]).}}
*)

  val htick : ?path:Vg.path -> ?halign:halign -> ?valign:valign -> ?pos:v2 ->
    float -> Vg.path
  (** [htick l] is a horizontal tick of length [l]. *)

  val vtick : ?path:Vg.path -> ?halign:halign -> ?valign:valign -> ?pos:v2 ->
    float -> Vg.path
  (** [vtick l] is a vertical tick of length [l]. *)

  val dot : ?path:Vg.path -> ?halign:halign ->
    ?valign:valign -> ?pos:v2 -> float -> Vg.path
  (** [dot w] is a dot of diameter [w]. *)

  val square : ?path:Vg.path -> ?halign:halign -> ?valign:valign -> ?pos:v2 ->
    float -> Vg.path
  (** [square w] is a square of side [w]. *)


(*

  val diamond : ?path:Vg.path -> ?halign:halign -> ?valign:valign -> ?pos:v2 ->
    float -> Vg.path
  (** [diamond w] is a diamond of side [w]. *)

  val plus : ?at:v2 -> float -> Vg.path
  (** [plus w] is a plus sign of length [w]. *)

  val cross : ?at:v2 -> float -> Vg.path
  (** [cros w] is a plus sign of length [w] rotated by 45°. *)
*)


end

(*
type axis

module Axis : sig

  type orientation = [ `Right | `Top | `Left | `Bottom ]
  type tick_spec = [ `Count of int | `Values of

  val of_scale : ?ticks:?orient:orientation -> ('a, 'b) scale ->

end
*)




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
