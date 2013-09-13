(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%

   ColorBrewer Color Schemes are Copyright 2002 Cynthia Brewer, Mark Harrower
   and the Pennsylvania State University. Licensed under the Apache License
   version 2.0, see http://www.apache.org/licenses/LICENSE-2.0

   The fixed qualitative color scheme `Wijffelaars_17 and the generated color
   schemes are taken from:
   
   M. Wijffelaars, Synthesis of Color Palettes, Technische Universiteit 
   Eindhoven, 2008.

   M. Wijfeelaars et al, Generating Color Palettes using Intuitive 
   Parameters, Eurographics 2008.
  ---------------------------------------------------------------------------*)

open Gg
open Vg

(* Invalid_arg strings *) 

let str = Printf.sprintf 
let err_scheme_size ssize s = str "scheme size %d exceeded (%d)" ssize s
let err_interval (lo, hi) = str "invalid interval: ... %f, %f ..." lo hi
let err_not_ipos n = str "%d is not a positive integer" n
let err_step s = str "step size is not positive (%f)" s
let err_bounds min max = 
  str "lower bound %f greater than upper bound %f" min max

(* Statistics *)

type ('a, 'b) stat = 
  { value : unit -> 'b;
    add : ('a, 'b) stat -> 'a -> ('a, 'b) stat; }

module Stat = struct

  (* Statistics *) 

  type ('a, 'b) t = ('a, 'b) stat      
  let add s v = s.add s v
  let add_flip v s = s.add s v
  let value s = s.value () 

  (* Primitive statistics *)

  let fcmp : float -> float -> int = Pervasives.compare
  let count =          
    let value s = 0. in
    let add s _ = 
      let count = s.value () in
      { s with value = fun () -> count +. 1. } 
    in
    { value; add }  
                  
  let min f =
    let value () = max_float in
    let add s v = 
      let v = f v in
      if fcmp v (s.value ()) < 0 then { s with value = fun () -> v } else s
    in
    { value; add }
    
  let max f =
    let value () = -. max_float in
    let add s v =
      let v = f v in
      if fcmp v (s.value ()) > 0 then { s with value = fun () -> v } else s
    in
    { value; add }
    
  let range f =
    let value () = max_float, -. max_float in
    let add s v = 
      let v = f v in 
      let min, max = s.value () in 
      let min', u1 = if fcmp v min < 0 then v, true else min, false in
      let max', u2 = if fcmp v max > 0 then v, true else max, false in
      if u1 || u2 then { s with value = fun () -> min', max' } else s
    in
    { value; add }
    
  let range_d (type e) ?(cmp = (Pervasives.compare : e -> e -> int)) f = 
    let module S = Set.Make(struct type t = e let compare = cmp end) in 
    let value () = [] in
    let rec add set s v = 
      let v = f v in 
      if S.mem v set then s else
      let set' = S.add v set in 
      { value = (fun () -> S.elements set'); add = (add set')  }
    in
    { value; add = add S.empty }

  let sum ?(nan = false) f =
    let value () = 0. in 
    let add s v =
      let v = f v in
      if not nan && Float.is_nan v then s else 
      let sum = s.value () +. v in
      { s with value = fun () -> sum }
    in 
    { value; add } 

  (* For calculating mean and variance we use Welford's algorithm, see
     the AOCP vol. 2. and http://dx.doi.org/10.2307/1266577 *)

  let mean ?(nan = false) f =
    let value () = 0. in 
    let rec add i s v = 
      let v = f v in
      if not nan && Float.is_nan v then s else 
      let m = s.value () in
      let i' = i +. 1. in 
      let m' = m +. (v -. m) /. i' in
      { value = (fun () -> m'); add = add i' }
    in
    { value; add = add 0. }
      
  let mean_var ?(nan = false) ?(pop = false) f = 
    let value () = 0., 0. in 
    let rec add i m s stat v = 
      let v = f v in
      if not nan && Float.is_nan v then stat else 
      let delta = v -. m in 
      let i' = i +. 1. in 
      let m' = m +. (delta /. i') in 
      let s' = s +. delta *. (v -. m') in 
      { value = (fun () -> m', s' /. (if pop then i' else i' -. 1.));
        add = add i' m' s'; }
    in
    { value; add = add 0. 0. 0. }
    
  let median ?(sorted = true) f = failwith "TODO"
  let quantile ?(sorted = true) p f = failwith "TODO"
  let fold f acc = 
    let value () = acc in 
    let add s v = 
      let acc' = f (s.value ()) v in 
      { s with value = fun () -> acc' } 
    in
    { value; add }

  (* Higher-order statistics *)

  let list l =
    let value () = List.map (fun s -> s.value ()) l in 
    let rec add l s v = 
      let l' = List.map (fun s -> s.add s v) l in 
      { add = add l'; value = fun () -> List.map (fun s -> s.value ()) l' }
    in
    { value; add = add l } 

  let t2 s1 s2 =
    let value () = s1.value (), s2.value () in 
    let rec add s1 s2 s v = 
      let s1' = s1.add s1 v in 
      let s2' = s2.add s2 v in 
      { add = add s1' s2'; value = fun () -> s1'.value (), s2'.value () }
    in
    { value; add = add s1 s2 }

  let t3 s1 s2 s3 =
    let value () = s1.value (), s2.value (), s3.value () in 
    let rec add s1 s2 s3 s v = 
      let s1' = s1.add s1 v in 
      let s2' = s2.add s2 v in 
      let s3' = s3.add s3 v in 
      { add = add s1' s2' s3'; 
        value = fun () -> s1'.value (), s2'.value (), s3'.value () }
    in
    { value; add = add s1 s2 s3 }    

  let t4 s1 s2 s3 s4 =
    let value () = s1.value (), s2.value (), s3.value (), s4.value () in 
    let rec add s1 s2 s3 s4 s v = 
      let s1' = s1.add s1 v in 
      let s2' = s2.add s2 v in 
      let s3' = s3.add s3 v in 
      let s4' = s4.add s4 v in 
      { add = add s1' s2' s3' s4'; 
        value = fun () -> s1'.value (), s2'.value (), s3'.value (), 
                          s4'.value (); }
    in
    { value; add = add s1 s2 s3 s4 }    

  let t5 s1 s2 s3 s4 s5 =
    let value () = s1.value (), s2.value (), s3.value (), s4.value (), 
                   s5.value () 
    in 
    let rec add s1 s2 s3 s4 s5 s v = 
      let s1' = s1.add s1 v in 
      let s2' = s2.add s2 v in 
      let s3' = s3.add s3 v in 
      let s4' = s4.add s4 v in 
      let s5' = s5.add s5 v in 
      { add = add s1' s2' s3' s4' s5'; 
        value = fun () -> s1'.value (), s2'.value (), s3'.value (), 
                         s4'.value (), s5'.value ()}
    in
    { value; add = add s1 s2 s3 s4 s5 }    
end

let linear_sample min max step = (* assert (min <= max) *)
  let countf = (max -. min) /. step in
  let count = 
    let count = Float.round countf in
    if Float.is_zero ~eps:step ((min +. step *. count) -. max) then count else 
    floor countf
  in
  let rec loop i acc = 
    if i < 0. then acc else loop (i -. 1.) ((min +. i *. step) :: acc)
  in
  loop count []

(* Nice numbers *)

module Nice = struct

  (* Draws from (but not the same as):
     
     P. Heckbert. Nice numbers for graph labels. 
     In A. Glassner, editor, Graphics Gems, pages 61–63 657–659. 
     Academic Press, Boston, 1990.
     
     Could be interesting to look at:
     
     An Extension of Wilkinson’s Algorithm for Positioning Tick Labels on Axes
     Justin Talbot, Sharon Lin, Pat Hanrahan
     IEEE Trans. Visualization & Comp. Graphics (Proc. InfoVis), 2010 *) 
      
  let nicenum ~round v = 
    let exp = floor (log10 v) in 
    let omag = 10. ** exp in 
    let frac = v /. omag in
    let nice_frac = 
      if round then begin
        if frac < 1.5 then 1. else 
        if frac < 3.5 then 2. else 
        if frac < 7.5 then 5. else
        10.
      end else begin 
        if frac <= 1. then 1. else 
        if frac <= 2. then 2. else 
        if frac <= 5. then 5. else 
        10.
      end
    in
    nice_frac *. omag

  let step n v0 v1 = 
    if n < 1 then invalid_arg (err_not_ipos n) else
    if n = 1 then nicenum ~round:true (0.5 *. abs_float (v1 -. v0)) else
    let ext = nicenum ~round:false (abs_float (v1 -. v0)) in
    nicenum ~round:true (ext /. (float (n - 1)))

  (* Quantized *)

  let step_floor ~step v = 
    if step <= 0. then invalid_arg (err_step step) else
    floor (v /. step) *. step
      
  let step_ceil ~step x = 
    if step <= 0. then invalid_arg (err_step step) else
    ceil (x /. step) *. step
      
  let step_round ~step x = 
    if step <= 0. then invalid_arg (err_step step) else
    Float.round (x /. step) *. step
      
  let step_bounds ~step ~inset v0 v1 =    
    if step <= 0. then invalid_arg (err_step step) else
    let v0 = 
      let v0r = step_round ~step v0 in 
      if Float.is_zero ~eps:(step *. 1e-9) (v0 -. v0r) then v0r else 
      if inset 
      then if v0 < v1 then step_ceil ~step v0 else step_floor ~step v0
      else if v0 < v1 then step_floor ~step v0 else step_ceil ~step v0
    in
    let v1 = 
      let v1r = step_round ~step v1 in 
      if Float.is_zero ~eps:(step *. 1e-9) (v1 -. v1r) then v1r else 
      if inset 
      then if v0 < v1 then step_floor ~step v1 else step_ceil ~step v1 
      else if v0 < v1 then step_ceil ~step v1 else step_floor ~step v1
    in
    v0, v1
    
  let step_fold ~step f acc v0 v1 = 
    if step <= 0. then invalid_arg (err_step step) else
    let prec = int_of_float (max (-. floor (log10 (abs_float (step)))) 0.) in
    if v0 = v1 then
      let v0r = step_round ~step v0 in 
      if Float.is_zero ~eps:(step *. 1e-9) (v0 -. v0r) then f acc prec v0 else 
      acc
    else
    let v0, v1 = step_bounds ~step ~inset:true v0 v1 in
    let exts = v1 -. v0 in
    let sstep = copysign step exts in
    let countf = exts /. sstep in
    let count = 
      let count = Float.round countf in
      if Float.is_zero ~eps:(step *. 1e-9) ((v0 +. sstep *. count) -. v1) 
      then count else floor countf
    in
    let rec loop i acc = 
      if i > count then acc else loop (i +. 1.) (f acc prec (v0 +. i *. sstep))
    in
    loop 0. acc
          
  let step_outset ~step v0 v1 = step_bounds ~step ~inset:false v0 v1 
  let step_inset ~step v0 v1 = step_bounds ~step ~inset:true v0 v1 
                                
end

(* Scale *)

module Scale = struct

  type 'a set = [ `Discrete of 'a list | `Intervals of 'a list ]
  type ('a, 'b) t = 
    { map : 'a -> 'b;
      dom : 'a set;
      dom_raw : 'a set;
      range : 'b set;
      clamp : bool;
      nice : bool; } 

  let clamp s = s.clamp
  let nice s = s.nice 
  let dom s = s.dom
  let dom_raw s = s.dom_raw
  let range s = s.range 
  let map s = s.map
  let partial_map s = 
    let f = s.map in
    fun v -> try Some (f v) with Invalid_argument _ -> None 
          
  let extents xs = match xs with 
  | (x0 :: xs) ->
      let rec last acc = function 
      | last :: [] -> last, acc 
      | x :: xs -> last (x :: acc) xs 
      | [] -> assert false
      in
      let xn, xs_rev = last [] xs in 
      x0, xn, xs_rev
  | [] -> assert false
            

  let nice_interval dom = 
    let x0, xn, xs_rev = extents dom in 
    if x0 = xn then dom else
    let exts = abs_float (xn -. x0) in
    let mag_order = 10. ** (Float.round (log10 exts) -. 1.) in  
    let rev = x0 > xn in
    let x0' = (if rev then ceil else floor) (x0 /. mag_order) *. mag_order in
    let xn' = (if rev then floor else ceil) (xn /. mag_order) *. mag_order in
    x0' :: List.rev (xn' :: xs_rev)

      
  let fold_ticks ?(bounds = false) n f acc scale = match scale.dom with 
  | `Discrete _ -> failwith "TODO discrete fold ticks" 
  | `Intervals [x0; x1] ->
      let step = Nice.step n x0 x1 in 
      if step = 0. then acc (* TODO *)
      else
      if not bounds then Nice.step_fold ~step f acc x0 x1 else
      let prec = int_of_float (max (-. floor (log10 (abs_float (step)))) 0.) in
      let x0', x1' = Nice.step_inset ~step x0 x1 in 
      let acc = f acc prec x0 in 
      let acc = Nice.step_fold ~step f acc x0' x1' in 
      f acc prec x1
  | _ -> assert false 
    

  let linear_map ~clamp d r = match d, r with 
  | [x0; x1], [y0; y1] -> 
      let inv_di = 1. /. (x1 -. x0) in
      let ri = y1 -. y0 in
      let c = inv_di *. ri in
      if x0 = x1 then fun t -> y0 else
      if not clamp then fun t -> c *. (t -. x0) +. y0 else
      fun t -> 
        if t > x1 then y1 else 
        if t < x0 then y0 else
        c *. (t -. x0) +. y0
  | _, _ -> failwith "TODO linmap"


  let linear ?(clamp = false) ?(nice = false) (dmin, dmax) (rmin, rmax) = 
    let dom_raw = [dmin; dmax] in 
    let range = [rmin; rmax] in
    let dom = if nice then nice_interval dom_raw else dom_raw in 
    { map = linear_map ~clamp dom range; 
      dom = `Intervals dom ; dom_raw = `Intervals dom_raw; 
      range = `Intervals range; clamp; nice }      
    
(* 
  let linear_p ?(clamp = false) ?(nice = false) dom range = 
    check_range dom; check_range range;
    failwith "TODO"
*)

  let ordinal_map (type dv) cmp d r = 
    let module Dmap = Map.Make(struct type t = dv let compare = cmp end) in
    let rec add acc d r' = match d, r' with 
    | [], _ -> acc
    | d, [] -> add acc d r 
    | x :: d, y :: r' -> add (Dmap.add x y acc) d r' 
    in
    let m = add Dmap.empty d r in 
    let f x = try Dmap.find x m with Not_found -> invalid_arg "TODO" in
    f
  
  let ordinal (type dv) ?(cmp = Pervasives.compare) (d : dv list) r =
    if r = [] then invalid_arg "TODO" else 
    { map = ordinal_map cmp d r; 
      dom = `Discrete d; 
      dom_raw = `Discrete d; 
      range = `Discrete r; clamp = false; nice = false }
    
  let range_pts ?rpad ~min ~max n = failwith "TODO"
  let range_bands ?rpad ?pad ~min ~max n = failwith "TODO"
end

type ('a, 'b) scale = ('a, 'b) Scale.t

module Mark = struct

  type halign = [ `Center | `Left | `Right ]
  (** The type for horizontal alignements. *)

  type valign = [ `Center | `Bottom | `Top ]
  (** The type for vertical alignements. *)
  
  let htick ?(path = P.empty) ?(halign = `Center) ?(valign = `Center) 
      ?(pos = P2.o) w =
    let xoff = match halign with 
    | `Center -> -.0.5 *. w | `Left -> -.w | `Right -> 0.
    in
    let p0 = P2.v (P2.x pos +. xoff) (P2.y pos) in
    let p1 = P2.v (P2.x pos +. xoff +. w) (P2.y pos) in
    path >> P.sub p0 >> P.line p1

  let vtick ?(path = P.empty) ?(halign = `Center) ?(valign = `Center) 
      ?(pos = P2.o) h =
    let yoff = match valign with 
    | `Center -> -.0.5 *. h | `Bottom -> -.h | `Top -> 0.
    in
    let p0 = P2.v (P2.x pos) (P2.y pos +. yoff) in
    let p1 = P2.v (P2.x pos) (P2.y pos +. yoff +. h) in
    path >> P.sub p0 >> P.line p1

  let dot ?(path = P.empty) ?(halign = `Center) ?(valign = `Center) 
      ?(pos = P2.o) w = 
    let hw = 0.5 *. w in
    let xoff = match halign with `Center -> 0. | `Left -> -.hw | `Right -> hw in
    let yoff = match valign with `Center -> 0. | `Bottom -> -.hw | `Top -> hw in
    let pos = P2.v (P2.x pos +. xoff) (P2.y pos +. yoff) in 
    path >> P.circle pos hw

  let square ?(path = P.empty) ?(halign = `Center) ?(valign = `Center) 
      ?(pos = P2.o) w =
    let xoff = match halign with 
    | `Center -> -.0.5 *. w | `Left -> -.w | `Right -> 0.
    in
    let yoff = match valign with 
    | `Center -> -.0.5 *. w | `Bottom -> -.w | `Top -> 0.
    in
    let pos = P2.v (P2.x pos +. xoff) (P2.y pos +. yoff) in 
    let rect = Box2.v pos (Size2.v w w) in
    path >> P.rect rect
    
end

module Path = struct
  let circle ?(c = P2.o) r = P.empty >> P.circle c r 
end


module Colors = struct

  let r_lch_uv_c = 179.0413773582776
  let y_lch_uv = V3.v 97.1392672 107.0642904 1.4987619    (* yellow LCH_uv. *)

  (* Quadratic interpolation *)
    
  let quad a b0 b1 b2 t = 
    let omt = 1. -. t in 
    let c0 = omt *. omt in 
    let c1 = 2. *. omt *. t in 
    let c2 = t *. t in 
    V3.(V4.v ((c0 *. (x b0)) +. (c1 *. (x b1)) +. (c2 *. (x b2)))
             ((c0 *. (y b0)) +. (c1 *. (y b1)) +. (c2 *. (y b2)))
             ((c0 *. (z b0)) +. (c1 *. (z b1)) +. (c2 *. (z b2)))
             a)

  let inv_quad b0 b1 b2 v =
    let c = b0 -. (2. *. b1) +. b2 in 
    let r = (b0 -. b1 +. sqrt ((b1 *. b1) -. (b0 *. b2) +. (c *. v))) /. c in 
    r

  (* Hue functions *)
      
  let mod_hue h = 
    let h = mod_float h Float.two_pi in 
    if h < 0. then h +. Float.two_pi else h

  let mix_hue h0 h1 t =          (* mixes hue using shortest path on circle. *)
    let d = (mod_hue (h1 -. h0 +. Float.pi)) -. Float.pi in
    mod_hue (h0 +. t *. d)

  let diff_hue h0 h1 = 
    let d = abs_float (h1 -. h0) in
    if d < Float.pi then d else Float.two_pi -. d
    
  let rgb2xyz = M3.v 0.4124564 0.3575761 0.1804375   (* TODO From Gg.Color. *)
                     0.2126729 0.7151522 0.0721750
                     0.0193339 0.1191920 0.9503041

  let msc h =    (* most satured color of [h], see Wijffelaars 2008, p. 36. *)
    let u'n = 0.1978398 in                           (* TODO From Gg.Color. *)
    let v'n = 0.4683363 in                           (* TODO From Gg.Color. *)
    let h0 = 0.21247617651602310 in           (* LCH_uv hue of RGB 1. 0. 0. *)
    let h1 = 1.49876191584179419 in           (* LCH_uv hue of RGB 1. 1. 0. *)
    let h2 = 2.22919630798637458 in           (* LCH_uv hue of RGB 0. 1. 0. *)
    let h3 = 3.35406860819474817 in           (* LCH_uv hue of RGB 0. 1. 1. *)
    let h4 = 4.64035469555325797 in           (* LCH_uv hue of RGB 0. 0. 1. *)
    let h5 = 5.37078918024889251 in           (* LCH_uv hue of RGB 1. 0. 1. *)
    let r, s, t = 
      if h0 <= h && h < h1 then 1, 2, 0 else
      if h1 <= h && h < h2 then 0, 2, 1 else
      if h2 <= h && h < h3 then 2, 0, 1 else 
      if h3 <= h && h < h4 then 1, 0, 2 else
      if h4 <= h && h < h5 then 0, 1, 2 else 
      if h5 <= h || h < h0 then 2, 1, 0 else 
      assert false
    in
    let alpha = -. sin h in
    let beta = cos h in
    let f = alpha *. u'n +. beta *. v'n in 
    let coeff p =
      let c0 = V3.(f *. ((x p) +. (15. *. (y p)) +. (3. *. (z p)))) in 
      let c1 = V3.((4. *. alpha *. (x p)) +. (9. *. beta *. (y p))) in 
      c0 -. c1 
    in
    let rv = -. (coeff (M3.col t rgb2xyz)) /. (coeff (M3.col r rgb2xyz)) in
    let a = [|0.; 0.; 0.|] in
    a.(r) <- rv; a.(s) <- 0.; a.(t) <- 1.;
    match a with [|r; g; b|] -> Color.v r g b 1.0 | _ -> assert false 
  
  (* Saturation functions. *)

  let max_s l h = 
    let pmid = Color.to_lch_uv (msc h) in 
    let pendL = if l <= V4.x pmid then 0. else 100. in
    let alpha = (l -. pendL) /. (V4.x pmid -. pendL) in 
    alpha *. V4.y pmid

  (* Sequential color schemes 
     See Wijffelaars 2008 p. 747 table 1. *)

  let p2_multi_hue w s h =            (* see Wijfelaars 2008 p. 747 table 2. *)
    let pb = y_lch_uv in
    let p2l = (1. -. w) *. 100. +. (w *. V3.x pb) in
    let p2h = mix_hue h (V3.z pb) w in
    let p2s = min (max_s p2l p2h) (w *. s *. V3.y pb) in 
    V3.v p2l p2s p2h    

  let seq ?(a = 1.) ?(w = 0.) ?(s = 0.6) ?(b = 0.75) ?(c = 0.88) ~h () = 
    let p0 = V3.v 0. 0. h in
    let p1 = V3.of_v4 (Color.to_lch_uv (msc h)) in 
    let p2 = if w > 0. then p2_multi_hue w s h else V3.v 100. 0. h in
    let q0 = V3.((1. -. s) * p0 + s * p1) in 
    let q2 = V3.((1. -. s) * p2 + s * p1) in 
    let q1 = V3.(0.5 * (q0 + q2)) in
    fun t ->
      let l t = 125. -. 125. *. (0.2 ** ((1. -. c) *. b +. t *. c)) in
      let t_ l =
        if l <= V3.x q1 
        then 0.5 *. (inv_quad (V3.x p0) (V3.x q0) (V3.x q1) l) 
        else 
          if l <= V3.x p2 then 
            0.5 *. (inv_quad (V3.x q1) (V3.x q2) (V3.x p2) l) +. 0.5
          else
          (* This can happen with w != 0., in which case inv_quad returns
             NaN, just return the max. *)
          1.
      in
      let cseq t = 
        if t <= 0.5 
        then quad a p0 q0 q1 (2. *. t) 
        else quad a q1 q2 p2 (2. *. (t -. 0.5))
      in
      Color.clamp (Color.of_lch_uv (cseq (t_ (l t))))
        
  let seq_d ?a ?w ?s ?b ?c ~h n = 
    let c = match c with 
    | None -> min 0.88 (0.34 +. (float n) *. 0.06) 
    | Some c -> c 
    in
    let seq = seq ?a ?w ?s ?b ~c ~h () in 
    let max = float (n - 1) in 
    Array.init n (fun i -> seq ((float i) /. max))

  (* Diverging color schemes 
     See Wijffelaars 2008 p. 53 table 1. *)

  let div ?(a = 1.) ?(w = 0.) ?(s = 0.6) ?(b = 0.75) ?(c = 0.88) ?(m = 0.5) 
      ~h0 ~h1 () = 
    let seq0 = seq ~a ~w ~s ~b ~c ~h:h0 () in 
    let seq1 = seq ~a ~w ~s ~b ~c ~h:h1 () in
    let e2 = 2. *. abs_float (m -. 0.5) in
    let t' = 
      if m < 0.5 then (fun t -> (t +. e2) /. (1. +. e2)) else
      fun t -> t /. (1. +. e2)
    in
    let cm = 
      let c0 = Color.to_lch_uv (seq0 1.) in 
      let c1 = Color.to_lch_uv (seq1 1.) in
      let l = 0.5 *. (V4.x c0 +. V4.x c1) in 
      let h = V3.z y_lch_uv in 
      let s = 
        let smax = w *. 0.5 *. (V4.y c0 +. V4.y c1) in 
        min (max_s l h) smax
      in
      Color.of_lch_uv (V4.v l s h a) 
    in
    fun t -> 
      let t' = t' t in 
      if Float.equal_tol ~eps:1e-8 t' 0.5 then cm else
      if t' < 0.5 then seq0 (2. *. t') else seq1 (2. *. (1. -. t')) 

  let div_d ?a ?w ?s ?b ?c ?(m = 0.5) ~h0 ~h1 n = 
    let c = match c with 
    | None -> min 0.88 (1.0 -. 0.06 *. (11. -. (float (n / 2 + 1))))
    | Some c -> c
    in
    let m' = floor (2. *. m *. ((float n) -. 1.) +. 0.5) /. 2. in
    if mod_float (2. *. m') 2. = 0. then 
      let max = float (n - 1) in 
      let div = div ?a ?w ?s ?b ~c ~m:(m' /. max) ~h0 ~h1 () in 
      Array.init n (fun i -> div ((float i) /. max))
    else
      let max = float n in
      let div = div ?a ?w ?s ?b ~c ~m:((m' +. 0.5) /. max) ~h0 ~h1 () in 
      Array.init n 
        (fun i -> 
           let i = float i in
           if i < m' +. 0.5 
           then div (i /. max)
           else div ((i +. 1.) /. max))
        
  (* Qualitative color schemes *)

  type qual_fixed = 
    [ `Brewer_accent_8 | `Brewer_dark2_8 | `Brewer_paired_12 
    | `Brewer_pastel1_9 | `Brewer_pastel2_8 | `Brewer_set1_9 
    | `Brewer_set2_8 | `Brewer_set3_12 | `Wijffelaars_17 ]

  let qual_fixed_size = function 
  | `Brewer_accent_8 -> 8 | `Brewer_dark2_8 -> 8 | `Brewer_paired_12 -> 12 
  | `Brewer_pastel1_9 -> 9 | `Brewer_pastel2_8 -> 8 | `Brewer_set1_9 -> 9 
  | `Brewer_set2_8 -> 8 | `Brewer_set3_12 -> 12 | `Wijffelaars_17 -> 17

  let rgb = Color.v_srgbi
  let brewer_accent_8 = lazy 
    [| rgb 127 201 127; rgb 190 174 212; rgb 253 192 134; rgb 255 255 153; 
       rgb 56  108 176; rgb 240 2   127; rgb 191 91   23; rgb 102 102 102; |]
    
  let brewer_dark2_8 = lazy
    [| rgb 27  158 119; rgb 217 95  2  ; rgb 117 112 179; rgb 231 41  138; 
       rgb 102 166 30 ; rgb 230 171 2  ; rgb 166 118 29 ; rgb 102 102 102; |]

  let brewer_paired_12 = lazy
    [| rgb 166 206 227; rgb 31  120 180; rgb 178 223 138; rgb 51  160 44 ; 
       rgb 251 154 153; rgb 227 26  28 ; rgb 253 191 111; rgb 255 127 0  ; 
       rgb 202 178 214; rgb 106 61  154; rgb 255 255 153; rgb 177 89  40 ; |]

  let brewer_pastel1_9 = lazy
    [| rgb 251 180 174; rgb 179 205 227; rgb 204 235 197; rgb 222 203 228; 
       rgb 254 217 166; rgb 255 255 204; rgb 229 216 189; rgb 253 218 236; 
       rgb 242 242 242; |]

  let brewer_pastel2_8 = lazy
    [| rgb 179 226 205; rgb 253 205 172; rgb 203 213 232; rgb 244 202 228; 
       rgb 230 245 201; rgb 255 242 174; rgb 241 226 204; rgb 204 204 204; |]

  let brewer_set1_9 = lazy
    [| rgb 228 26  28 ; rgb 55  126 184; rgb 77  175 74 ; rgb 152 78  163; 
       rgb 255 127 0  ; rgb 255 255 51 ; rgb 166 86  40 ; rgb 247 129 191; 
       rgb 153 153 153; |]
    
  let brewer_set2_8 = lazy
    [| rgb 102 194 165; rgb 252 141 98 ; rgb 141 160 203; rgb 231 138 195; 
       rgb 166 216 84 ; rgb 255 217 47 ; rgb 229 196 148; rgb 179 179 179; |]

  let brewer_set3_12 = lazy
    [| rgb 141 211 199; rgb 255 255 179; rgb 190 186 218; rgb 251 128 114; 
       rgb 128 177 211; rgb 253 180 98 ; rgb 179 222 105; rgb 252 205 229;
       rgb 217 217 217; rgb 188 128 189; rgb 204 235 197; rgb 255 237 111; |]
       
  let wijffelaars_17 = lazy
    [| rgb 92  107 247; rgb 255 89  89 ; rgb 92  203 92 ; rgb 255 177 17 ;
       rgb 170 97  187; rgb 255 255 95 ; rgb 255 137 235; rgb 145 101 62 ; 
       rgb 193 193 193; rgb 92  229 214; rgb 201 255 135; rgb 255 224 204; 
       rgb 173  45  92; rgb 227 196 239; rgb 226 212 149; rgb 204 241 255; 
       rgb  87 142  82; |]

  let qual_fixed ?(a = 1.) ?size q = 
    let qsize = qual_fixed_size q in
    let size = match size with 
    | None -> qsize 
    | Some s -> if s > qsize then invalid_arg (err_scheme_size qsize s); s
    in
    let scheme = match q with 
    | `Brewer_accent_8 -> brewer_accent_8 
    | `Brewer_dark2_8 -> brewer_dark2_8 
    | `Brewer_paired_12 -> brewer_paired_12 
    | `Brewer_pastel1_9 -> brewer_pastel1_9 
    | `Brewer_pastel2_8 -> brewer_pastel2_8
    | `Brewer_set1_9 -> brewer_set1_9
    | `Brewer_set2_8 -> brewer_set2_8
    | `Brewer_set3_12 -> brewer_set3_12
    | `Wijffelaars_17 -> wijffelaars_17 
    in
    let colors = Array.sub (Lazy.force scheme) 0 size in
    if a = 1. then colors else Array.map (fun c -> Color.with_a c a) colors
    

  let qual ?(a = 1.) ?(eps = 0.) ?(r = 1.) ?(s = 0.5) ?(b = 1.) ?(c = 0.5) () = 
    fun t ->                                 (* see Wijffelaars 2008, p. 65. *)
      let h = mod_hue ((V3.z y_lch_uv) +. Float.two_pi *. (eps +. t *. r)) in
      let alpha = (diff_hue h (V3.z y_lch_uv)) /. Float.two_pi in
      let l0 = b *. V3.x y_lch_uv in
      let l1 = (1. -. c) *. l0 in
      let l = (1. -. alpha) *. l0 +. alpha *. l1 in
      let s = min (max_s l h) (s *. r_lch_uv_c) in
      Color.clamp (Color.of_lch_uv (V4.v l s h a))
      
  let qual_d ?a ?eps ?r ?s ?b ?c n = 
    let qual = qual ?a ?eps ?r ?s ?b ?c () in
    let max = float n in
    Array.init n (fun i -> qual ((float i) /. max))      
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
