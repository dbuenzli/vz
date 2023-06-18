(*---------------------------------------------------------------------------
   Copyright (c) 2023 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** HTML canvas tools. *)

open Gg
open Gg_kit
open Brr_canvas

(** Making styles with {!Gg} types. *)
module C2d_style : sig
  val of_color : Color.t -> C2d.style
  (** [of_color c] is a style for [c]. *)
end

(** Making paths out of {!Gg} types. *)
module C2d_path : sig

  val dot : w:float -> P2.t -> C2d.Path.t
  (** [dot pt] is a circle of width [w] centered on [pt]. *)

  val box2 : Box2.t -> C2d.Path.t
  (** [box2 b] is a path for [b]. *)

  val ring2 : Ring2.t -> C2d.Path.t
  (** [ring2 r] is a path for [r]. *)

  val pgon2 : Pgon2.t -> C2d.Path.t
  (** [pgon2 p] is a path for [p]. *)
end

(** Debug tools. *)
module C2d_debug : sig
  val time : string -> (unit -> 'a) -> 'a
  val path_dir : w:float -> P2.t list -> C2d.Path.t
end

(** Setting up view rectangles. *)
module C2d_viewrect : sig

  (** {1:align Alignements} *)

  type h_align = [ `Left | `Center | `Right | `Fit ]
  (** The type for horizontal alignments. *)

  type v_align = [ `Top | `Center | `Bottom | `Fit ]
  (** The type for vertical alignements. *)

  val aspect : C2d.t -> float
  (** [aspect ctx] is the current aspect of [ctx]. *)

  val set : ?h_align:h_align -> ?v_align:v_align -> C2d.t -> view:box2 -> unit
  (** [set ctx ~view] setups [ctx]'s coordinate system such that
      the bottom left corner of [view] is on the bottom left of [ctx]'s
      surface and the top right corner of [view] on the top right.

      By default the map is such that the aspect ratio is preserved
      and the whole [view] rectangle is maximally visible. If the view
      and surface aspect mismatch, the smaller view gets aligned in
      the surface according to [h_align] or [v_align] (defaults to
      [`Left] and [`Bottom]). If [`Fit] is used the aspect ratio is
      not preserved and the resulting view is stretched.

      {b Note.} The function does not clip the view. *)
end

(** Rectangles of pixels.

    {b FIXME.} Not sure all that is so useful maybe we just want
    {!get}, {!get_irect} and {!put}.

    This module helps with dealing with pixel data in image and canvas
    space:
    {ul
    {- The {e image space} is the coordinate system with origin at the
       top left of the canvas surface and that runs from left to right and top
       to bottom along the pixels of the surface.}
    {- The {e canvas space} is defined by the current value of
       {!Brr_canvas.C2d.get_transform} of the canvas context.}} *)
module C2d_pixrect : sig

  (** {1:pixels Pixels} *)

  type t
  (** The type for pixel rectangles.

      This packs an
      {{:https://developer.mozilla.org/en-US/docs/Web/API/ImageData}[ImageData]}
      object with information about the rectangular region it represents in the
      canvas space {b when it was created}. The rectangular region is
      represented by a {!Gg.box2} value which is a bottom-left origin and
      extents to the right and to the top. *)

  val make : ?color_space:Jstr.t -> ?rect:box2 -> C2d.t -> t
  (** [make ctx ~rect] are transparent black pixels for the
      rectangular region [rect] expressed in the current canvas space
      of [ctx] (defaults to the surface of the canvas). *)

  val get : ?color_space:Jstr.t -> ?rect:box2 -> C2d.t -> t
  (** [get ctx ~rect] are the current pixels for the rectangular
      region [rect] in the current canvas space of [ctx] (defaults to
      the surface of the canvas). *)

  (** {1:props Properties} *)

  (** {2:image_space Image space} *)

  val ix : t -> int
  (** [ix pr] is the x image space coordinate of the top left pixel of [pr]. *)

  val iy : t -> int
  (** [iy pr] is the y image space coordinate of the top left pixel of [pr]. *)

  val iw : t -> int
  (** [iw pr] is the pixel width of [pr]. *)

  val ih : t -> int
  (** [ih pr] is the pixel height of [pr]. *)

  val irect : t -> box2
  (** [irect] is {!ix}, {!iy}, {!iw}, {!ih} as a box. *)

  val pixel_size : t -> Size2.t
  (** [pixel_size pr] is the horizontal and vertical size of a pixel
      in the canvas space of [pr]. *)

(** {2:canvas_space Canvas space}

    {b Note.} The canvas space of pixel rectangles is the canvas space
    that was in place when the pixel rectangle was created. It does
    not reflect subsequent updates made to the canvas context. *)

  val rect : t -> box2
  (** [rect pr] is the canvas space rectangular region corresponding
      to [pr]. *)

  val ppu : t -> Size2.t
  (** [ppu pr] is the horizontal and vertical resolution
      in pixels (dots) per unit of canvas space of [pr]. *)

  (** {2:image_data Image data} *)

  val color_space : t -> Jstr.t
  (** [color_space pr] is the color space of [pr].  *)

  val data : t -> Brr.Tarray.uint8_clamped
  (** [data pr] is the pixel data of [pr]. The size of the array is
      [ih * iw * 4]. Pixels are from left to right top to bottom in
      RGBA order in the given {!colorspace} (sRGB by default, be
      careful {!Gg.Color.t} is linear). The first pixel corresponds to
      the top left corner of {!rect} and the last one to the bottom
      right. *)

  val image_data : t -> C2d.Image_data.t
  (** [image_data ps] is the underlying [ImageData] object. *)

  (** {1:drawing Drawing} *)

  val put : ?o:P2.t -> C2d.t -> t -> unit
  (** [put ctx ~o pr] puts the pixels so that the bottom-left pixel is at
      [o] in [ctx]'s {e current} canvas space (which may be different
      from the one of [pr]). If [o] is unspecified {!ix}, {!iy} are used
      to specify the location of the top left pixel. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2023 The vz programmers

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
