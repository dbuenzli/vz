(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Declarative data visualization with [Vg]. 

    [Vz] helps you to map data to [Vg] images. 


    Open the module to use it, this defines only modules
    and types in your scope.

    {e Release %%VERSION%% - %%MAINTAINER%% } *)

open Gg
open Vg

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

(** {1:scales Scales} *)

type ('a, 'b) scale
(** The type for scales from values of type ['a] to ['b]. *)

(** Scales.

    [Scale] represent functions mapping domains to ranges.  They help
    to map dimensions of data to dimensions of visual representations.
    Scales can also be represented as images. *) 
module Scale : sig

  (** {1 Scales} *)

  type 'a set = [ `Discrete of 'a list | `Intervals of 'a list ] 
  (** The type for representing sets of values. *)
  
  type ('a, 'b) t = ('a, 'b) scale
  (** The type for scales from values of type ['a] to ['b]. *)

  val clamp : ('a, 'b) scale -> bool 
  (** [clamp s] is [true] if the map of [s] is clamped to the scale
      domain. *)

  val nice : ('a, 'b) scale -> bool
  (** [nice s] is [true] if the bounds of [s] are niced. *)

  val dom : ('a, 'b) scale -> 'a set
  (** [dom s] is the (possibly niced) domain of [s]. *)

  val dom_raw : ('a, 'b) scale -> 'a set
  (** [dom s] is the unniced domain of [s]. *)
  
  val range : ('a, 'b) scale -> 'b set
  (** [range s] is the range of [s]. *)

  val map : ('a, 'b) scale -> ('a -> 'b)
  (** [map s] is the mapping function of [s]. 

      {b Warning.} On [s] ordinal scales the mapping function raises
      [Invalid_argument] on undefined argument. Use {!partial_map} to
      ensure that it never raises. *)

  val partial_map : ('a, 'b) scale -> ('a -> 'b option) 
  (** [partial_map s] is like [map s] except on ordinal scales 
      it returns [None] on undefined argument. *)

  val ticks : ?count:bool -> ('a, 'b) scale -> 'a list
      
  (** {1 Linear scales} *)

  (** The type for linear scales. Linear scales maps *)

  val linear : ?clamp:bool -> ?nice:bool -> (float * float) -> 
    (float * float) -> (float, float) scale
  (** [linear clamp nice dom range] maps the interval [dom] on 
      the interval [range]. 
      {ul 
      {- [clamp] if [false] (default), the scale maps values outside
         the domain according to the specified linear
         transformation. If [true] values outside the domain are
         clamped to the nearest domain bounds.}
      {- [nice] if [true] the bounds of [dom] are {e expanded} to fall
         on round numbers. The precision of these round numbers is one
         order of magnitude less than the extent [d] of the domain that
         is: 10{^(round (log{_10} d) - 1)}. Default to [false].}}  

      The map is undefined if [(fst dom) >= (snd dom)]. TODO review that. *)

(* 
  val linear_p : ?clamp:bool -> ?nice:bool -> float list -> float list -> 
    (float, float) scale 
  (** [linear_p clamp nice dom range] maps numbers according to the 
      piecewise linear function defined by dom and range (which must
      have the same length). See {!linear}. *)
*)
  (** {1 Ordinal scales} 

      Ordinal scales maps a discrete orderable domain to a range. *)

  val ordinal : ?cmp:('a -> 'a -> int) -> 'a list -> 'b list -> ('a, 'b) scale
  (** [ordinal cmp dom range] maps the value [dom]{_i} to the value 
      [range]{_i mod max} with max [= List.length range - 1]. 
      [cmp] is the order on the domain, defaults to [Pervasives.compare]. *)

  

(** {1 Generating discrete ranges} *)

  val range_pts : ?rpad:float -> min:float -> max:float -> int -> float list
  val range_bands : ?rpad:float -> ?pad:float -> min:float -> max:float ->
    int -> float list     
end


(** {1 Image helpers} *) 

module Path : sig
  val circle : ?c:v2 -> float -> Vg.path
end

(** {1 Colors schemes} *)

(** Color schemes. 

    [Colors] provides functions to generate continuous and discrete
    color schemes to map quantitative or qualitative data to colors.
    
    {b References.}
    {ul 
    {- M. Wijffelaars et al. 
    {{:http://dx.doi.org/10.1111/j.1467-8659.2008.01203.x}
    {e Generating Color Palettes using Intuitive Parameters}}, 2008.}
    {- C. Brewer. {{:http://www.colorbrewer.org}www.colorbrewer.org}, 2002.}} *)
module Colors : sig

  (** {1:sequential Sequential color schemes} 

      Sequential color schemes are for ordered scalar data. *)

  val seq : ?a:float -> ?w:float -> ?s:float -> ?b:float -> ?c:float -> 
    h:float -> unit -> (float -> Gg.color)
  (** [seq a w s b c h ()] is a function mapping the unit interval \[[0;1]\]
      to colors with a continuous sequential scheme where [0] is 
      the darkest color and [1] the lightest. The parameters are:
      {ul
      {- [h] in \[[0;2pi]\] the main hue, the overall color.}
      {- [w] in \[[0;1]\] is the hue warmth for a multi-hue scheme, 
         defaults to [0] (single-hue scheme). Augmenting [w] adds
         yellow which makes the scheme warmer.}
      {- [s] in \[[0;1]\] is saturation, the overall colorfullness, 
         defaults to [0.6].}
      {- [b] in \[[0;1]\] is brightness, the overall lightness, defaults to 
         [0.75].}
      {- [c] in \[[0;1]\] is contrast, the lightness difference
         between the darkest and the ligthest colors of the scheme, 
         defaults to [0.88].}
      {- [a] is the alpha component, defaults to [1.].}}

      {b Note.} For equal [b], [c] and [w = 0], sequential schemes
      with different hues [h] have the same lightness. This can be
      used to generate multiple sequential schemes for multivariate
      data. *)

  val seq_d : ?a:float -> ?w:float -> ?s:float -> ?b:float -> ?c:float -> 
    h:float ->  int -> Gg.color array
  (** [seq_d a w s b c h n] is like {!seq} except it
      returns a discrete sequential scheme with [n] colors and
      [c] defaults to [min 0.88 (0.34 +. 0.06. * n)]. *)

  (** {1:diverging Diverging color schemes} 

      Diverging color schemes are for ordered scalar data with a
      defined midpoint (e.g. zero or the data average). *)

  val div : ?a:float -> ?w:float -> ?s:float -> ?b:float -> ?c:float -> 
    ?m:float -> h0:float -> h1:float -> unit -> (float -> Gg.color)
  (** [div a w s b c m h0 h1 ()] is a function mapping the unit interval 
      \[[0;1]\] to colors for a continuous diverging scheme with [0] returning
      the darkest color of [h0], and [1] the darkest color of [h1].
      {ul
      {- [h0] in \[[0;2pi]\] is the hue, the overall color for lower values.}
      {- [h1] in \[[0;2pi]\] is the hue, the overall color for higher values.}
      {- [w] in \[[0;1]\] is the hue warmth for a multi-hue scheme, 
         defaults to [0] (single-hue scheme). Augmenting [w] adds
         yellow which makes the scheme warmer.}
      {- [s] in \[[0;1]\] is saturation, the overall colorfullness, 
         defaults to [0.6].}
      {- [b] in \[[0;1]\] is brightness, the overall lightness, defaults to 
         [0.75].}
      {- [c] in \[[0;1]\] is contrast, the lightness difference
         between the darkest and the ligthest colors of the scheme, 
         defaults to [0.88].}
      {- [m] is the mid point position, defaults to [0.5].}
      {- [a] is the alpha component, defaults to [1.].}} *)

  val div_d : ?a:float -> ?w:float -> ?s:float -> ?b:float -> ?c:float -> 
    ?m:float -> h0:float -> h1:float -> int -> Gg.color array
  (** [div_d a w s b c m h0 h1 n] is like {!div} except it returns 
      a discrete diverging scheme with [n] colors and [c] defaults 
      to [min 0.88 (1.0 - 0.06 *. (11 - ((n / 2) + 1)))]. *)

  (** {1:qualitative Qualitative color schemes} 

      Qualitative color schemes are used for qualitative (categorical,
      nominal) data. *)

  type qual_fixed = 
    [ `Brewer_accent_8 | `Brewer_dark2_8 | `Brewer_paired_12 
    | `Brewer_pastel1_9 | `Brewer_pastel2_8 | `Brewer_set1_9 
    | `Brewer_set2_8 | `Brewer_set3_12 | `Wijffelaars_17 ]

  (** The type for qualitative color scheme with fixed colors. The
      suffix indicates the maximal number of colors in the scheme. *)

  val qual_fixed_size : qual_fixed -> int 
  (** [qual_fixed_size q] is the maximal number of colors in [qf]. *)

  val qual_fixed : ?a:float -> ?size:int -> qual_fixed -> Gg.color array
  (** [qual_fixed size q] is fixed qualitative color scheme [q]
      with [size] colors (defaults to [qual_fixed_size q]) and alpha 
      component [a] (defaults to [1]). 

      @raise Invalid_argument if [size] is greater than 
      [qual_fixed_size b]. *)

  val qual_d : ?a:float -> ?eps:float -> ?r:float -> ?s:float -> ?b:float -> 
    ?c:float -> int -> Gg.color array
  (** [qual_d eps r s b c n] is a qualitative scheme with [n] colors. The
      parameters are:
      {ul
      {- [eps] in \[[0;1]\] is the hue shift, defines where the range of hues 
         begin, defaults to [0] (yellow).}
      {- [r] in \[[0;1]\] is the used hue range proportion, defaults to [1].}
      {- [s] in \[[0;1]\] is saturation, the overall colorfullness, 
         defaults to [0.5].}
      {- [b] in \[[0;1]\] is brightness, the overall lightness, defaults to 
         [1].}
      {- [c] in \[[0;1]\] is contrast, the lightness difference
         between the darkest and the ligthest colors of the scheme, 
         defaults to [0.5].}
      {- [a] is the alpha component, defaults to [1.].}} *)
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
