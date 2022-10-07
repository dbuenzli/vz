(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Vg

(* Nice numbers *)

module Nice = struct
  let strf = Printf.sprintf
  let err_not_pos n = strf "%d is not > 0" n
  let err_step s = strf "step size %f is not > 0" s

  (* Draws from (but not the same as):

     P. Heckbert. Nice numbers for graph labels.
     In A. Glassner, editor, Graphics Gems, pages 61–63 657–659.
     Academic Press, Boston, 1990.

     Could be interesting to look at:

     An Extension of Wilkinson’s Algorithm for Positioning Tick Labels on Axes
     Justin Talbot, Sharon Lin, Pat Hanrahan
     https://doi.org/10.1109/TVCG.2010.130 *)

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
    if n < 1 then invalid_arg (err_not_pos n) else
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

module Dom = struct
  module Shape = struct
    type 'a t = ('a -> 'a) * ('a -> 'a)
    let v f ~inv = f, inv
    let linear = v Fun.id ~inv:Fun.id
    let log ~base = match base with
    | 10. -> v Float.log10 ~inv:(fun x -> Float.pow 10. x)
    | base ->
        let logb' ~base =
          let inv_logb = 1. /. (Float.log base) in
          fun x -> (Float.log x) *. inv_logb
        in
        v (logb' ~base) ~inv:(fun x -> Float.pow base x)

    let sq x = x *. x
    let pow ~exp =
      let cb x = x *. x *. x in
      match exp with
      | 2. -> v sq ~inv:Float.sqrt
      | 0.5 -> v Float.sqrt ~inv:sq
      | 3. -> v cb ~inv:Float.cbrt
      | exp when exp = (1. /. 3.) -> v Float.cbrt ~inv:cb
      | exp ->
          let pow ~exp x = Float.pow x exp in
          let inv_pow ~exp = let inv = 1. /. exp in fun x -> Float.pow x inv in
          v (pow ~exp) ~inv:(inv_pow ~exp)

    let sqrt = v Float.sqrt ~inv:sq
    let inv (f, inv_f) = v inv_f ~inv:f
  end

  let nice_bounds ~rmin ~rmax = (* XXX review I don't think this makes sense *)
    let d = abs_float (rmax -. rmin) in
    let mag_order = 10. ** (Float.round (log10 d) -. 1.) in
    let min = floor @@ (rmin /. mag_order) *. mag_order in
    let max = ceil @@ (rmax /. mag_order) *. mag_order in
    min, max

  type 'a compare = 'a -> 'a -> int
  type 'a fmt = Format.formatter -> 'a -> unit
  type piecewise_proj = Proportional | Uniform
  type kind = Discrete | Continuous of piecewise_proj
  type 'a t =
    { clamped : bool;
      compare : 'a compare;
      inj : 'a -> float;
      kind : kind;
      niced : bool;
      none : 'a;
      pp : 'a fmt;
      proj : float -> 'a;
      proj_extents : float -> 'a * 'a;
      unniced_values : 'a list;
      values : 'a list }

  let v
      ?(clamped = false) ?(compare = Stdlib.compare) ?proj_extents
      ?unniced_values ?pp kind ~none values inj proj
    =
    let pp = match pp with
    | None -> fun ppf _ -> Format.pp_print_string ppf "<value>" | Some pp -> pp
    in
    let niced, unniced_values = match unniced_values with
    | None -> false, values | Some vs -> true, vs
    in
    let proj_extents = match proj_extents with
    | None -> fun t -> let v = proj t in v, v | Some extents -> extents
    in
    { clamped; compare; inj; kind; niced; none; pp; proj; proj_extents;
      values; unniced_values; }

  let clamped d = d.clamped
  let compare d = d.compare
  let kind d = d.kind
  let inj d = d.inj
  let niced d = d.niced
  let none d = d.none
  let pp d = d.pp
  let proj d = d.proj
  let proj_extents d = d.proj_extents
  let unniced_values d = d.unniced_values
  let values d = d.values

  let v_bounds ~clamped ~nice ~map ~inv_map ~rmin ~rmax =
    let min, max = if nice then nice_bounds ~rmin ~rmax else rmin, rmax in
    let map = map ~min ~max and inv_map = inv_map ~min ~max in
    let values = [min; max] in
    let unniced_values = [rmin; rmax] in
    let pp = fun ppf f -> Format.fprintf ppf "%g" f in
    let kind = Continuous Proportional in
    let none = Float.nan in
    v ~clamped ~compare:Float.compare ~unniced_values ~pp kind ~none values
      map inv_map

  let linear' ~min ~max =
    let inv_d = 1. /. (max -. min) in
    fun x -> (x -. min) *. inv_d

  let inv_linear' ~min ~max =
    let d = max -. min in
    fun x -> min +. (x *. d)

  let linear_clamped ~min ~max =
    let inv_d = 1. /. max -. min in
    fun x ->
      if Float.compare x min < 0 then 0. else
      if Float.compare x max > 0 then 1. else
      (x -. min) *. inv_d

  let inv_linear_clamped ~min ~max =
    let d = max -. min in
    fun x ->
      if Float.compare x 0. < 0 then min else
      if Float.compare x 1. > 0 then max else
      min +. (x *. d)

  let linear ?(nice = false) ?(clamp = false) rmin rmax =
    let map, inv_map = match clamp with
    | true -> linear_clamped, inv_linear_clamped
    | false -> linear', inv_linear'
    in
    v_bounds ~clamped:clamp ~nice ~map ~inv_map ~rmin ~rmax

  (* XXX Maybe some expansion of [func_*] could improve perfs. *)

  let func f ~min ~max =
    let fmin = f min and fmax = f max in
    let inv_d = 1. /. (fmax -. fmin) in
    fun x -> (f x -. fmin) *. inv_d

  let func_inv f f_inv ~min ~max =
    let fmin = f min and fmax = f max in
    let d = fmax -. fmin in
    fun x -> f_inv ((x +. fmin) *. d)

  let func_clamped f ~min ~max =
    let fmin = f min and fmax = f max in
    let inv_d = 1. /. (fmax -. fmin) in
    fun x ->
      if Float.compare x min < 0 then 0. else
      if Float.compare x max > 0 then 1. else
      (f x -. fmin) *. inv_d

  let func_inv_clamped f f_inv ~min ~max =
    let fmin = f min and fmax = f max in
    let d = fmax -. fmin in
    fun x ->
      if Float.compare x 0. < 0 then min else
      if Float.compare x 1. > 0 then max else
      f_inv ((x +. fmin) *. d)

  let log ?(base = 10.) ?(nice = false) ? (clamp = false) rmin rmax =
    let logb, inv_logb  = match base with
    | 10. -> Float.log10, fun x -> Float.pow 10. x
    | base ->
        let logb' ~base =
          let inv_logb = 1. /. (Float.log base) in
          fun x -> (Float.log x) *. inv_logb
        in
        logb' ~base, fun x -> Float.pow base x
    in
    let map, inv_map = match clamp with
    | true -> func_clamped logb, func_inv_clamped logb inv_logb
    | false -> func logb, func_inv logb inv_logb
    in
    v_bounds ~clamped:clamp ~nice ~map ~inv_map ~rmin ~rmax

  let pow ?(exp = 10.) ?(nice = false) ? (clamp = false) rmin rmax =
    let sq x = x *. x and cb x = x *. x *. x in
    let pow, inv_pow = match exp with
    | 2. -> sq, Float.sqrt | 0.5 -> Float.sqrt, sq
    | 3. -> cb, Float.cbrt | v when exp = (1. /. 3.) -> Float.cbrt, cb
    | exp ->
        let pow ~exp x = Float.pow x exp in
        let inv_pow ~exp = let inv = 1. /. exp in fun x -> Float.pow x inv in
        pow ~exp , inv_pow ~exp
    in
    let map, inv_map = match clamp with
    | true -> func_clamped pow, func_inv_clamped pow inv_pow
    | false -> func pow, func_inv pow inv_pow
    in
    v_bounds ~clamped:clamp ~nice ~map ~inv_map ~rmin ~rmax

  let sqrt = pow ~exp:0.5


  let inj a v = failwith "TODO"

  let discrete ?(compare = Stdlib.compare) ?pp ?none vs =
    if vs = [] then invalid_arg "Empty domain" else
    let a = let a = Array.of_list vs in Array.sort compare a; a in
    let none = match none with None -> a.(Array.length a - 1) | Some e -> e in
    let inj v =
      let len = Array.length a in
      let max = len - 1 in
      if compare v a.(0) < 0 then 0. else
      if compare v a.(max) > 0 then (float max) /. (float len) else
      let rec loop a lo hi v =
        if lo = hi then (float lo) /. (float len) else
        let mid = (lo + hi) / 2 in
        let c = compare v a.(mid) in
        if c < 0 then loop a lo (mid - 1) v else
        if c > 0 then loop a (mid + 1) hi v else
        (float mid) /. (float len)
      in
      loop a 0 max v
    in
    let proj t =
      if Float.compare t 0. < 0 then a.(0) else
      if Float.compare t 1. >= 0 then none else
      let max = float (Array.length a - 1) in
      a.(Float.to_int (t *. max))
    in
    let proj_extents t = failwith "TODO" in
    v ~clamped:true ~compare ~proj_extents ?pp Discrete ~none vs inj proj

end

module Dmap = struct
  (* TODO optimize discrete maps to direct maps. *)
  type ('a, 'b) t = { dom : 'a Dom.t; codom : 'b Dom.t }
  let v dom codom = { dom; codom }
  let dom m = m.dom
  let codom m = m.codom
  let map m = fun v ->  m.codom.proj (m.dom.inj v)
  let inv_map m = fun v -> m.dom.proj (m.codom.inj v)
  let inv { dom; codom } = { dom = codom; codom = dom }
  let compose m0 m1 = failwith "TODO"
  let merge m0 m1 = failwith "TODO"
end

(* Scale *)

module Scale = struct
  type 'a set =
  [ `Discrete of 'a list
  | `Continuous of 'a list ]

  let nice_bounds ~rmin ~rmax = (* XXX review I don't think this makes sense*)
    let d = abs_float (rmax -. rmin) in
    let mag_order = 10. ** (Float.round (log10 d) -. 1.) in
    let min = floor @@ (rmin /. mag_order) *. mag_order in
    let max = ceil @@ (rmax /. mag_order) *. mag_order in
    min, max

  module Dom = struct

    (* TODO add symlog
       10.1088/0957-0233/24/2/027001 *)

    type 'a t =
      { map : 'a -> float;
        inv_map : (float -> 'a) option;
        set : 'a set;
        niced_set : 'a set; }

    let v ~map ~inv_map ~set ~niced_set =
      { map; inv_map; set; niced_set; }

    let v_bounds ~nice ~map ~inv_map ~rmin ~rmax =
      let min, max = if nice then nice_bounds ~rmin ~rmax else rmin, rmax in
      let map = map ~min ~max and inv_map = Some (inv_map ~min ~max) in
      let niced_set = `Continuous [min; max] in
      let set = `Continuous [min;max] in
      { map; inv_map; set; niced_set }

    let linear' ~min ~max =
      let inv_d = 1. /. (max -. min) in
      fun x -> (x -. min) *. inv_d

    let inv_linear' ~min ~max =
      let d = max -. min in
      fun x -> min +. (x *. d)

    let linear_clamped ~min ~max =
      let inv_d = 1. /. max -. min in
      fun x ->
        if Float.compare x min < 0 then 0. else
        if Float.compare x max > 0 then 1. else
        (x -. min) *. inv_d

    let inv_linear_clamped ~min ~max =
      let d = max -. min in
      fun x ->
        if Float.compare x 0. < 0 then min else
        if Float.compare x 1. > 0 then max else
        min +. (x *. d)

    let linear ?(nice = false) ?(clamp = false) rmin rmax =
      let map, inv_map = match clamp with
      | true -> linear_clamped, inv_linear_clamped
      | false -> linear', inv_linear'
      in
      v_bounds ~nice ~map ~inv_map ~rmin ~rmax

    (* XXX Maybe some expansion of [func_*] could improve perfs. *)

    let func f ~min ~max =
      let fmin = f min and fmax = f max in
      let inv_d = 1. /. (fmax -. fmin) in
      fun x -> (f x -. fmin) *. inv_d

    let func_inv f f_inv ~min ~max =
      let fmin = f min and fmax = f max in
      let d = fmax -. fmin in
      fun x -> f_inv ((x +. fmin) *. d)

    let func_clamped f ~min ~max =
      let fmin = f min and fmax = f max in
      let inv_d = 1. /. (fmax -. fmin) in
      fun x ->
        if Float.compare x min < 0 then 0. else
        if Float.compare x max > 0 then 1. else
        (f x -. fmin) *. inv_d

    let func_inv_clamped f f_inv ~min ~max =
      let fmin = f min and fmax = f max in
      let d = fmax -. fmin in
      fun x ->
        if Float.compare x 0. < 0 then min else
        if Float.compare x 1. > 0 then max else
        f_inv ((x +. fmin) *. d)

    let log ?(base = 10.) ?(nice = false) ? (clamp = false) rmin rmax =
      let logb, inv_logb  = match base with
      | 10. -> Float.log10, fun x -> Float.pow 10. x
      | base ->
          let logb' ~base =
            let inv_logb = 1. /. (Float.log base) in
            fun x -> (Float.log x) *. inv_logb
          in
          logb' ~base, fun x -> Float.pow base x
      in
      let map, inv_map = match clamp with
      | true -> func_clamped logb, func_inv_clamped logb inv_logb
      | false -> func logb, func_inv logb inv_logb
      in
      v_bounds ~nice ~map ~inv_map ~rmin ~rmax

    let pow ?(exp = 10.) ?(nice = false) ? (clamp = false) rmin rmax =
      let sq x = x *. x and cb x = x *. x *. x in
      let pow, inv_pow = match exp with
      | 2. -> sq, Float.sqrt | 0.5 -> Float.sqrt, sq
      | 3. -> cb, Float.cbrt | v when exp = (1. /. 3.) -> Float.cbrt, cb
      | exp ->
          let pow ~exp x = Float.pow x exp in
          let inv_pow ~exp = let inv = 1. /. exp in fun x -> Float.pow x inv in
          pow ~exp , inv_pow ~exp
      in
      let map, inv_map = match clamp with
      | true -> func_clamped pow, func_inv_clamped pow inv_pow
      | false -> func pow, func_inv pow inv_pow
      in
      v_bounds ~nice ~map ~inv_map ~rmin ~rmax

    let sqrt = pow ~exp:0.5
    let map d = d.map
    let inv_map d = d.inv_map
  end

  module Range = struct
    type 'a t =
      { map : float -> 'a;
        inv_map : ('a -> float) option;
        set : 'a set;
        niced_set : 'a set; }

    let v_bounds ~nice ~map ~inv_map ~rmin ~rmax =
      let min, max = if nice then nice_bounds ~rmin ~rmax else rmin, rmax in
      let map = map ~min ~max and inv_map = Some (inv_map ~min ~max) in
      let niced_set = `Continuous [min; max] in
      let set = `Continuous [min;max] in
      { map; inv_map; set; niced_set }

    let linear = Dom.inv_linear'
    let inv_linear = Dom.linear'

    let bounds ?(nice = false) rmin rmax =
      v_bounds ~nice ~map:linear ~inv_map:inv_linear ~rmin ~rmax


    let map d = d.map
    let inv_map d = d.inv_map
  end

  type ('a, 'b) t =
    { map : 'a -> 'b;
      inv_map : ('b -> 'a) option;
      dom : 'a Dom.t;
      range : 'b Range.t }

  let v dom range =
    let map =
      let dmap = Dom.map dom and rmap = Range.map range in
      fun x -> rmap (dmap x)
    in
    let inv_map = match Range.inv_map range, Dom.inv_map dom with
    | Some inv_r, Some inv_d -> Some (fun y -> inv_d (inv_r y))
    | _ -> None
    in
    { map; inv_map; dom; range }

  let dom s = s.dom
  let range s = s.range
  let map s = s.map
  let inv_map s = s.inv_map
end


module Scale' = struct
  type 'a set =
  [ `Discrete of 'a list
  | `Continuous of 'a list ]


  type ('a, 'b) t =
    { map : 'a -> 'b;
      dom : 'a set;
      dom_raw : 'a set;
      range : 'b set;
      clamp : bool;
      nice : bool; }

  let clamp s = s.clamp
  let niced s = s.nice
  let dom s = s.dom
  let dom_unniced s = s.dom_raw
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
  | `Continuous [x0; x1] ->
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
      dom = `Continuous dom ; dom_raw = `Continuous dom_raw;
      range = `Continuous range; clamp; nice }

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

  let ordinal (type dv) ?(cmp = Stdlib.compare) (d : dv list) r =
    if r = [] then invalid_arg "TODO" else
    { map = ordinal_map cmp d r;
      dom = `Discrete d;
      dom_raw = `Discrete d;
      range = `Discrete r; clamp = false; nice = false }

  let range_pts ?rpad ~min ~max n = failwith "TODO"
  let range_bands ?rpad ?pad ~min ~max n = failwith "TODO"
end

(* Invalid_arg strings *)

let str = Printf.sprintf
let err_scheme_size ssize s = str "scheme size %d exceeded (%d)" ssize s
let err_interval (lo, hi) = str "invalid interval: ... %f, %f ..." lo hi
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

  let fcmp : float -> float -> int = Stdlib.compare
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

  let range_d (type e) ?(cmp = (Stdlib.compare : e -> e -> int)) f =
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
    path |> P.sub p0 |> P.line p1

  let vtick ?(path = P.empty) ?(halign = `Center) ?(valign = `Center)
      ?(pos = P2.o) h =
    let yoff = match valign with
    | `Center -> -.0.5 *. h | `Bottom -> -.h | `Top -> 0.
    in
    let p0 = P2.v (P2.x pos) (P2.y pos +. yoff) in
    let p1 = P2.v (P2.x pos) (P2.y pos +. yoff +. h) in
    path |> P.sub p0 |> P.line p1

  let dot ?(path = P.empty) ?(halign = `Center) ?(valign = `Center)
      ?(pos = P2.o) w =
    let hw = 0.5 *. w in
    let xoff = match halign with `Center -> 0. | `Left -> -.hw | `Right -> hw in
    let yoff = match valign with `Center -> 0. | `Bottom -> -.hw | `Top -> hw in
    let pos = P2.v (P2.x pos +. xoff) (P2.y pos +. yoff) in
    path |> P.circle pos hw

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
    path |> P.rect rect

end

module Path = struct
  let circle ?(c = P2.o) r = P.empty |> P.circle c r
end

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
