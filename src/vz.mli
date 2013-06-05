(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Visualize data with [Vg]. 

    [Vz] helps you to map data to [Vg] images. 

    {e Release %%VERSION%% - %%AUTHORS%% } 
*)

open Gg
open Vg

(** {1 Map data to colors} *)

(** Color schemes. 

    [Color] provides functions to generate continuous and discrete
    color schemes to map quantitative or qualitative data to colors.

    {b References.}
    {ul 
    {- M. Wijffelaars et al. 
    {{:http://dx.doi.org/10.1111/j.1467-8659.2008.01203.x}
    {e Generating Color Palettes using Intuitive Parameters}}, 2008.}
    {- C. Brewer. {{:http://www.colorbrewer.org}www.colorbrewer.org}, 2002.}}
*)
module Color : sig

  (** {1:sequential Sequential color schemes} 

      Sequential color schemes are for ordered scalar data. *)

  val seq : ?w:float -> ?s:float -> ?b:float -> ?c:float -> h:float ->  
    (float -> Gg.color)
  (** [seq w s b c h] is a function mapping the unit interval \[[0;1]\]
      to colors with a continuous sequential scheme where [0] is 
      the darkest color and [1] the lightest. The parameters are:
      {ul
      {- [h] in \[[0;2pi]\] the main hue, the overall color.}
      {- [w] in \[[0;1]\] is the hue warmth for a multi-hue scheme, 
         defaults to [0] (single-hue scheme).}
      {- [s] in \[[0;1]\] is saturation, the overall colorfullness, 
         defaults to [0.6].}
      {- [b] in \[[0;1]\] is brightness, the overall lightness, defaults to 
         [0.75].}
      {- [c] in \[[0;1]\] is contrast, the lightness difference
         between the darkest and the ligthest colors of the scheme, 
         defaults to [0.88].}} *)

  val seq_d : ?w:float -> ?s:float -> ?b:float -> ?c:float -> h:float ->  
    int -> Gg.color array
  (** [seq_d w s b c h n] is like {!seq} except it
      returns a discrete sequential scheme with [n] colors and
      [c] defaults to [min 0.88 (0.34 +. 0.06. * n)]. *)

  (** {1:diverging Diverging color schemes} 

      Diverging color schemes are for ordered scalar data with a
      defined midpoint (e.g. zero or the data average). *)

  val div : ?w:float -> ?s:float -> ?b:float -> ?c:float -> ?m:float -> 
    h0:float -> h1:float -> (float -> Gg.color)
  (** [div w s b c m h0 h1] is a function mapping the unit interval 
      \[[0;1]\] to colors for a continuous diverging scheme with [0] returning
      the darkest color of [h0], and [1] the darkest color of [h1].
      {ul
      {- [m] is the mid point position, defaults to [0.5].}
      {- [h0] in \[[0;2pi]\] is the hue, the overall color for lower values.}
      {- [h1] in \[[0;2pi]\] is the hue, the overall color for higher values.}
      {- [w] in \[[0;1]\] is the hue warmth for a multi-hue scheme, 
         defaults to [0] (single-hue scheme).}
      {- [s] in \[[0;1]\] is saturation, the overall colorfullness, 
         defaults to [0.6].}
      {- [b] in \[[0;1]\] is brightness, the overall lightness, defaults to 
         [0.75].}
      {- [c] in \[[0;1]\] is contrast, the lightness difference
         between the darkest and the ligthest colors of the scheme, 
         defaults to [0.88].}} *)

  val div_d : ?w:float -> ?s:float -> ?b:float -> ?c:float -> ?m:Gg.color -> 
    h0:float -> h1:float -> int -> Gg.color array
  (** [div_d w s b c m h0 h1 n] is like {!div} except it returns 
      a discrete diverging scheme with [n] colors and [c] defaults 
      to [min 0.88 (1.0 - 0.06 *. (11 - ((n / 2) + 1)))]. *)

  (** {1:qualitative Qualitative color schemes} 

      Qualitative color schemes are used for qualitative (categorical,
      nominal) data. *)

  type qual_fixed = 
    [ `Brewer_accent_8 
    | `Brewer_dark2_8 
    | `Brewer_paired_12 
    | `Brewer_pastel1_8 
    | `Brewer_pastel2_8 
    | `Brewer_set1_9
    | `Brewer_set2_8 
    | `Brewer_set3_12 
    | `Wijffelaars_17 ]
  (** The type for qualitative color scheme with fixed colors. The
      suffix indicates the maximal number of colors in the scheme. *)

  val qual_fixed_size : qual_fixed -> int 
  (** [qual_fixed_size q] is the maximal number of colors in [qf]. *)

  val qual_fixed : ?size:int -> qual_fixed -> Gg.color array
  (** [qual_fixed size q] is fixed qualitative color scheme [q]
      with [size] colors (defaults to [qual_fixed_size q]).

      @raise Invalid_argument if [size] is greater than 
      [qual_fixed_size b]. *)

  val qual_d : ?eps:float -> ?r:float -> ?s:float -> ?b:float -> ?c:float -> 
    int -> Gg.color array
  (** [qual_d eps r s b c n] is a qualitative scheme with [n] colors. The
      parameters are:
        {ul
      {- [eps] in \[[0;1]\] is the hue shift, defines where the range of hues 
         begin, defaults to [0].}
      {- [r] in \[[0;1]\] is the used hue range, defaults to [1].}
      {- [s] in \[[0;1]\] is saturation, the overall colorfullness, 
         defaults to [0.5].}
      {- [b] in \[[0;1]\] is brightness, the overall lightness, defaults to 
         [1].}
      {- [c] in \[[0;1]\] is contrast, the lightness difference
         between the darkest and the ligthest colors of the scheme, 
         defaults to [0.5].}} *)
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
