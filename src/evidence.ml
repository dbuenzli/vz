(*---------------------------------------------------------------------------
   Copyright (c) 2022 The evidence programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type 'a fmt = Format.formatter -> 'a -> unit

module Fmt = struct
  let str = Format.asprintf
  let pf = Format.fprintf
  let repeat n pp_v ppf v = for i = 1 to n do pp_v ppf v done
  let bold pp_v ppf v = pf ppf "@<0>%s%a@<0>%s" "\027[01m" pp_v v "\027[m"
  let red pp_v ppf v = pf ppf "@<0>%s%a@<0>%s" "\027[31m" pp_v v "\027[m"
  let bool = Format.pp_print_bool
  let int = Format.pp_print_int
  let char = Format.pp_print_char
  let string = Format.pp_print_string
  let bold_int = bold int
  let dots ppf () = pf ppf "@<1>%s" "…"
  let cross ppf () = pf ppf "@<1>%s" "⨯"
  let unknown ppf v = Format.pp_print_string ppf "<??>"
  let tbreak = Format.pp_print_tbreak
  let dump_option pp_v ppf = function
  | None -> string ppf "None" | Some v -> pf ppf "@[Some@ @[%a@]@]" pp_v v

  let floatg ppf v =
    if Float.is_nan v then red string ppf "nan" else
    Format.fprintf ppf "%g" v
end

module Id = struct
  type t = int
  let nil = -1
  let equal = Int.equal
  let compare = Int.compare
  let make =
    let id = Atomic.make (nil + 1) in fun () -> Atomic.fetch_and_add id 1
end

(* Type identifiers *)

module Tid = struct
  (* See http://alan.petitepomme.net/cwn/2015.03.24.html#1 *)

  type _ id = ..
  module type ID = sig type t type _ id += V : t id end
  type 'a t = (module ID with type t = 'a)

  let make () (type s) =
    let module T = struct type t = s type _ id += V : t id end in
    (module T : ID with type t = s)

  type ('a, 'b) eq = Eq : ('a, 'a) eq
  let equal (type t0) (type t1) (t0 : t0 t) (t1 : t1 t) : (t0, t1) eq option
    =
    let module T0 = (val t0 : ID with type t = t0) in
    let module T1 = (val t1 : ID with type t = t1) in
    match T0.V with T1.V -> Some Eq | _ -> None
end


module Var = struct

  (* Variable types *)

  type nonrec 'a fmt = 'a fmt
  type 'a type' =
  | Bool : bool type'
  | Int : int type'
  | Float : float type'
  | Ordinal : ('a -> 'a -> int) -> 'a type'
  | Nominal : string type'
  | Any : 'a type'

  (* XXX maybe expose these instances *)

  module Bset = Set.Make (Bool)   module Bmap = Map.Make (Bool)
  module Iset = Set.Make (Int)    module Imap = Map.Make (Int)
  module Fset = Set.Make (Float)  module Fmap = Map.Make (Float)
  module Nset = Set.Make (String) module Nmap = Map.Make (String)

  module Type = struct
    type 'a t = 'a type'

    let is_numeric : type a. a type' -> bool = function
    | Bool | Int | Float -> true | _ -> false

    let pp : type a. Format.formatter -> a type' -> unit =
    fun ppf k ->  Fmt.string ppf @@ match k with
    | Bool -> "bool" | Int -> "int" | Float -> "float"
    | Nominal -> "nominal" | Ordinal _ -> "ordinal" | Any -> "any"

    let value_compare : type a. a type' -> (a -> a -> int) = function
    | Bool -> Bool.compare | Int -> Int.compare | Float -> Float.compare
    | Nominal -> String.compare | Ordinal compare -> compare
    | Any -> Stdlib.compare

    let value_pp : type a. a type' -> a fmt = function
    | Bool -> Fmt.bool | Int -> Fmt.int | Float -> Fmt.floatg
    | Nominal -> Fmt.string | Ordinal _ -> Fmt.unknown  | Any -> Fmt.unknown

    let value_set : type a set. a type' -> (module Set.S with type elt = a) =
    function
    | Float -> (module Fset) | Int -> (module Iset) | Bool -> (module Bset)
    | Nominal -> (module Nset)
    | Ordinal compare ->
        let module V = struct type t = a let compare = compare end in
        (module (Set.Make (V)))
    | Any ->
        let module V = struct type t = a let compare = Stdlib.compare end in
        (module (Set.Make (V)))

    let value_map : type a. a type' -> (module Map.S with type key = a) =
    function
    | Float -> (module Fmap) | Bool -> (module Bmap) | Int -> (module Imap)
    | Nominal -> (module Nmap)
    | Ordinal compare ->
        let module V = struct type t = a let compare = compare end in
        (module (Map.Make (V)))
    | Any ->
        let module V = struct type t = a let compare = Stdlib.compare end in
        (module (Map.Make (V)))
  end

  (* Variables *)

  type name = string
  type 'o v = V : ('o, 'a) t -> 'o v
  and ('o, 'a) t =
    { doc : string;
      exist : 'o v;
      id : Id.t;
      tid : 'a Tid.t;
      type' : 'a type';
      label : string;
      name : name;
      pp : 'a fmt;
      proj : 'o -> 'a; }

  let make ?(doc = "") ?label ?pp name type' proj =
    let pp = match pp with Some pp -> pp | None -> Type.value_pp type' in
    let label = Option.value ~default:name label in
    let id = Id.make () and tid = Tid.make () in
    let rec exist = V var
    and var = {doc; exist; id; tid; type'; label; name; pp; proj} in
    var

  let v ?doc ?label ?pp name type' proj = make ?doc ?label ?pp name type' proj
  let bool ?doc ?label ?pp name proj = make ?doc ?label ?pp name Bool proj
  let int ?doc ?label ?pp name proj = make ?doc ?label ?pp name Int proj
  let float ?doc ?label ?pp name proj = make ?doc ?label ?pp name Float proj
  let nominal ?doc ?label ?pp name proj = make ?doc ?label ?pp name Nominal proj
  let ordinal ?doc ?label ?pp name cmp proj =
    make ?doc ?label ?pp name (Ordinal cmp) proj

  let any ?doc ?label ?pp name proj = make ?doc ?pp ?label name Any proj
  let const ?doc ?label ?pp name type' v =
    make ?doc ?pp ?label name type' (Fun.const v)

  let id type' = make "id" type' Fun.id

  (* Properties *)

  let doc var = var.doc
  let exist var = var.exist
  let label var = var.label
  let name var = var.name
  let pp var = var.pp
  let proj var = var.proj
  let type' var = var.type'
  let equal var0 var1 = Int.equal var0.id var1.id
  let compare var0 var1 = Int.compare var0.id var1.id
  let pp ppf var =
    Fmt.pf ppf "@[<h>%a %s : %a@]"
      Fmt.(bold string) "var" var.name Fmt.(bold Type.pp) var.type'

  (* Transforming *)

  let alter ?doc ?label ?pp ?name var =
    let doc = Option.value ~default:var.doc doc in
    let label = Option.value ~default:var.label label in
    let name = Option.value ~default:var.name name in
    let pp = Option.value ~default:var.pp pp in
    let rec exist = V var'
    and var' = { var with doc; label; name; pp; exist } in var'

  let map ?doc ?label ?pp f var name type' =
    let proj o = f (var.proj o) in make ?doc ?label ?pp name type' proj

  let with_proj ?doc ?label ?pp ?name var proj =
    let doc = Option.value ~default:var.doc doc in
    let label = Option.value ~default:var.label label in
    let name = Option.value ~default:var.name name in
    let pp = Option.value ~default:var.pp pp in
    make ~doc ~pp ~label name (var.type') proj

  (* Operations on variable values *)

  let compare_value var = Type.value_compare var.type'
  let equal_value var =
    let compare = compare_value var in
    fun v0 v1 -> compare v0 v1 = 0

  let min_value : type a. ('o, a) t -> (a -> a -> a) =
  fun var -> match var.type' with
  | Float -> Float.min_num
  | k ->
      let compare = Type.value_compare var.type' in
      fun v0 v1 -> if compare v0 v1 <= 0 then v0 else v1

  let max_value : type a. ('o, a) t -> (a -> a -> a) =
  fun var -> match var.type' with
  | Float -> Float.max_num
  | k ->
      let compare = Type.value_compare var.type' in
      fun v0 v1 -> if compare v0 v1 <= 0 then v1 else v0

  let pp_value var = var.pp

  (* Operations on observations *)

  type 'a order =
  | A : ('o, 'a) t -> 'o order
  | D : ('o, 'a) t -> 'o order

  let err_empty = "var list for Var.order cannot be empty"
  let check_non_empty_order = function [] -> invalid_arg err_empty | _ -> ()

  (* XXX a few alternate implementations should be measured here.
         E.g. hoisting the (Var.compare_value var) result *)

  let rec unchecked_order vars o1 o2 = match vars with
  | [] -> 0
  | (A var) :: vars ->
      let c = compare_value var (var.proj o1) (var.proj o2) in
      if c = 0 then unchecked_order vars o1 o2 else c
  | (D var) :: vars ->
      let c = compare_value var (var.proj o2) (var.proj o1) in
      if c = 0 then unchecked_order vars o1 o2 else c

  let order vars = match (check_non_empty_order vars; vars) with
  | [A var] -> fun o1 o2 -> compare_value var (var.proj o1) (var.proj o2)
  | [D var] -> fun o1 o2 -> compare_value var (var.proj o2) (var.proj o1)
  | vars -> unchecked_order vars

  let is_na var = fun o -> Float.is_nan (var.proj o)

  let[@inline] fbt v0 v1 v = Float.(compare v0 v <= 0 && compare v v1 <= 0)
  let[@inline] ibt v0 v1 v = Int.(compare v0 v <= 0 && compare v v1 <= 0)
  let[@inline] obt cmp v0 v1 v = cmp v0 v <= 0 && cmp v v1 <= 0
  let[@inline] bbt v0 v1 v = Bool.(compare v0 v <= 0 && compare v v1 <= 0)
  let[@inline] nbt v0 v1 v = String.(compare v0 v <= 0 && compare v v1 <= 0)
  let[@inline] gbt v0 v1 v = Stdlib.(compare v0 v <= 0 && compare v v1 <= 0)
  let between : type a. a -> a -> ('o, a) t -> ('o -> bool) =
  fun v0 v1 var -> match var.type' with
  | Float -> fun o -> fbt v0 v1 (var.proj o)
  | Int -> fun o -> ibt v0 v1 (var.proj o)
  | Ordinal compare -> fun o -> obt compare v0 v1 (var.proj o)
  | Bool -> fun o -> bbt v0 v1 (var.proj o)
  | Nominal -> fun o -> nbt v0 v1 (var.proj o)
  | Any -> fun o -> gbt v0 v1 (var.proj o)

  let equal_obs var =
    let compare = compare_value var in
    fun o0 o1 -> compare (var.proj o0) (var.proj o1) = 0

  let compare_obs var =
    let compare = compare_value var in
    fun o0 o1 -> compare (var.proj o0) (var.proj o1)

  let fmin_obs var o0 o1 =
    (* Adapted from Float.min_num *)
    let (x : float) = var.proj o0 and (y : float) = var.proj o1 in
    if y > x || (not (Float.sign_bit y) && Float.sign_bit x)
    then if Float.is_nan x then o1 else o0
    else if Float.is_nan y then o0 else o1

  let min_obs :
    type a. ('o, a) t -> ('o -> 'o -> 'o) = fun var ->
    match var.type' with
    | Float -> fmin_obs var
    | kind ->
        let compare = Type.value_compare var.type' in
        fun o0 o1 -> if compare (var.proj o0) (var.proj o1) <= 0 then o0 else o1

  let fmax_obs var o0 o1 =
    (* Adapted from Float.max_num *)
    let (x : float) = var.proj o0 and (y : float) = var.proj o1 in
    if y > x || (not (Float.sign_bit y) && Float.sign_bit x)
    then (if Float.is_nan y then o0 else o1)
    else (if Float.is_nan x then o1 else o0)

  let max_obs : type a. ('o, a) t -> ('o -> 'o -> 'o) =
  fun var -> match var.type' with
  | Float -> fmax_obs var
  | kind ->
      let compare = Type.value_compare var.type' in
      fun o0 o1 -> if compare (var.proj o0) (var.proj o1) <= 0 then o1 else o0

  let pp_obs var ppf o = pp_value var ppf (var.proj o)

  (* Standard variables *)

  let for_var_count var =
    let fst = v ~doc:var.doc ~pp:var.pp var.name var.type' fst in
    let snd = v "count" Int snd in
    fst, snd

  let for_var_values var =
    let doc = "domain of " ^ var.name in
    v ~doc ~pp:var.pp var.name var.type' Fun.id

  let for_var var = v ~doc:var.doc ~pp:var.pp var.name var.type' Fun.id
  let cumsum = float "cumsum" Fun.id

  module Prod = struct
    (* Would a CPS final encoding be possible and more efficent ? *)

    type ('o, 'p) var = ('o, 'p) t

    type ('o, 'p) t =
    | Unit : 'p -> ('o, 'p) t
    | Prod : ('o, 'a -> 'b) t * ('o, 'a) var -> ('o, 'b) t

    let unit f = Unit f
    let prod p var = Prod (p, var)

    let ( * ) = prod

    let absurd () = Unit (fun _ -> raise (Invalid_argument "absurd product"))

    let vars p =
      let rec loop : type o a. o v list -> (o, a) t -> o v list =
      fun acc -> function
      | Unit _ -> acc
      | Prod (p, var) -> loop (var.exist :: acc) p
      in
      loop [] p
  end


  module Gprod = struct
    (* Would a CPS final encoding be possible and more efficent ? *)

    type ('o, 'p) var = ('o, 'p) t
    type ('c, 'o, 'p) t =
    | Unit : 'p -> ('c, 'o, 'p) t
    | Prod : ('c, 'o, 'a -> 'b) t * ('o, 'a) var -> ('c, 'o, 'b) t

    let unit f = Unit f
    let prod prod var = Prod (prod, var)
    let ( * ) = prod

    let rec proj :
      type c o p. (c, o, p) t -> o -> p
      = fun prod o -> match prod with
      | Unit f -> f
      | Prod (prod, var) -> let f = proj prod o in f (var.proj o)
  end

  (* Codec

     FIXME needs a field in variable. *)

  let pbool s = bool_of_string s (* raises Invalid_argument *)
  let pfloat s = float_of_string s
  let pint = function "NA" -> 0 (* TODO *) | s -> int_of_string s
  let pstring s = Fun.id s

  let text_decoder : type a. ('o, a) t -> (string -> a) =
  fun var -> match type' var with
  | Bool -> pbool
  | Int -> pint
  | Float -> pfloat
  | Nominal -> pstring
  | Ordinal _ | Any -> (* TODO *)
      invalid_arg "Need a text decoder for Any or Ordinal variable"

  let text_encoder : type a. ('o, a) t -> (a -> string) =
  fun var -> match type' var with
  | Bool -> Bool.to_string
  | Int -> Int.to_string
  | Float -> fun v -> Printf.sprintf "%.17g" v
  | Nominal -> Fun.id
  | Ordinal _ | Any -> (* TODO *)
      invalid_arg "Need a text encoder for Any or Ordinal variable"
end

module Obs = struct
  type 'o t = { doc : string; prod : ('o, 'o) Var.Prod.t; vars : 'o Var.v list }
  let v ?(doc = "") prod = { doc; prod; vars = Var.Prod.vars prod }
  let empty () =
    let empty _ = invalid_arg "empty observation" in
    let var = Var.any "empty" Fun.id in
    { doc = "empty"; prod = Var.Prod.(unit empty * var); vars = [] }

  let doc obs = obs.doc
  let prod obs = obs.prod
  let vars obs = obs.vars
  let compare obs = Var.order (List.map (fun (Var.V v) -> Var.A v) obs.vars)
  let set : type o a. o t -> (o, a) Var.t -> a -> o -> o = fun obs var v o ->
    let rec loop : type o b . (o, b) Var.Prod.t -> o -> b =
    fun prod o -> match prod with
    | Var.Prod.Unit f -> f
    | Var.Prod.Prod (prod, var') ->
        let f = loop prod o in
        match Tid.equal var.tid var'.tid with
        | None -> f (var'.proj o)
        | Some Tid.Eq -> f v
    in
    loop obs.prod o

  let t1 = Fun.id
  let t2 v0 v1 = v0, v1
  let o1 ?doc var = v ?doc Var.Prod.(unit t1 * var)
  let o2 ?doc var0 var1 = v ?doc Var.Prod.(unit t2 * var0 * var1)
end

module Dataset = struct
  (* Fancy stuff could be done here.
     - Na values via index array.
     - Filtering via index array.
     - Cache for variable queries
     - Columns as arrays storage for variable processing (either implictely
       or explicitely).
    or none of that. *)

  let uget = Array.get (* eventually unsafe that *)
  let uset = Array.set (* eventually unsafe that *)

  let err_empty = "Dataset cannot be empty"
  let err_take = "Cannot take 0 observations"
  let err_bounds i min max = Fmt.str "%d not in range [%d;%d]" i min max
  let err_range_spec ?first ?last ?count length =
    Fmt.str "?first:%a ?last:%a ?count:%a is empty for range [0;%d]"
      Fmt.(dump_option int) first Fmt.(dump_option int) last
      Fmt.(dump_option int) count (length - 1)

  let range_spec ?(first = 0) ?last ?count length =
    let first = Int.max 0 first in
    let last = match last with
    | None -> length - 1 | Some last -> Int.min last (length - 1)
    in
    let range_size = last - first + 1 in
    if range_size <= 0 then 0, 0 else match count with
    | None -> first, range_size
    | Some c when c < 0 -> Int.max first (last + c + 1), -c
    | Some c -> first, Int.min c range_size

  (* Extensible arrays *)

  module Buffer = struct
    type 'a t = { mutable vs : 'a array; mutable max : int; }
    let make ~size v = { vs = Array.make size v; max = -1 }
    let length b = b.max + 1
    let grow b =
      let len = b.max + 1 in
      let vs' = Array.make (2 * len) b.vs.(0) in
      Array.blit b.vs 0 vs' 0 len; b.vs <- vs'

    let add b v =
      let max = b.max + 1 in
      if max = Array.length b.vs then grow b;
      b.max <- max; uset b.vs max v

    let rec add_array b a =
      let alen = Array.length a in
      let max = b.max + alen in
      if max >= Array.length b.vs then (grow b; add_array b a) else
      Array.blit a 0 b.vs (b.max + 1) alen; b.max <- max

    let to_array b =
      let len = b.max + 1 in
      if len = 0 then [||] else
      if Array.length b.vs = len then b.vs else
      let a = Array.make len b.vs.(0) in
      Array.blit b.vs 0 a 0 len; a
  end

  (* Datasets *)

  type rand = int -> int
  type count = int
  type 'o t = { obs : 'o Obs.t; os : 'o array;  }

  let make ?obs os =
    let obs = match obs with None -> Obs.empty () | Some obs -> obs in
    if Array.length os < 0 then invalid_arg err_empty else { obs; os }

  let init ?obs n init = make ?obs (Array.init n init)
  let reobs obs d = { d with obs }

  (* Properties *)

  let obs d = d.obs
  let length d = Array.length d.os
  let vars d = (Obs.vars d.obs)
  let get i d =
    if i < 0 || i >= length d - 1
    then invalid_arg (err_bounds i 0 (length d - 1)) else uget d.os i

  (* Traversing *)

  let iter f d = Array.iteri f d.os
  let fold f d acc =
    let acc = ref acc and max = length d - 1 in
    for i = 0 to max do acc := f i (uget d.os i) !acc done; !acc

  (* Transforming *)

  let map ?obs f d =
    let os = Array.make (length d) (f 0 (uget d.os 0)) in
    for i = 1 to length d - 1 do uset os i (f i (uget d.os i)) done;
    make ?obs os

  let filter_map ?obs f d =
    let first =
      let rec loop os max i =
        if i > max then None else match f i (uget os i) with
        | None -> loop os max (i + 1)
        | Some o -> Some (o, i)
      in
      loop d.os (length d) 0
    in
    match first with
    | None -> None
    | Some (o, k) ->
        let b = Buffer.make ~size:(length d - k) o in
        for i = k + 1 to length d - 1 do match f i (uget d.os i) with
        | None -> () | Some o -> Buffer.add b o
        done;
        Some (make ?obs (Buffer.to_array b))

  let update upd d =
    let os = Array.make (length d) (upd 0 (uget d.os 0)) in
    for i = 1 to length d - 1 do uset os i (upd i (uget d.os i)) done;
    make ~obs:(obs d) os

  let set i o d =
    if i < 0 || i >= length d
    then invalid_arg (err_bounds i 0 (length d - 1))
    else (let os = Array.copy d.os in uset os i o; { d with os })

  (* Formatting
     TODO improve. Limit column size (currently based on name).
     Copy cat R more, limit column size and size them based on the data.
     Footnotes for truncated. *)

  let idx_pad = 3
  let pp_pad ppf pad = Fmt.repeat pad Fmt.char ppf ' '

  let pp_dim ppf idx_width d =
    let len_width = 1 + (Float.to_int @@ log10 (float (length d))) in
    let pad = idx_pad - (len_width - idx_width) - 2 in
    Fmt.pf ppf "%a%a%a %a"
      Fmt.bold_int (length d) pp_pad pad Fmt.cross ()
      Fmt.bold_int (List.length (vars d))

  let pp_var_dots ppf vars =
    let pp_col_dots ppf () = Fmt.dots ppf (); Fmt.tbreak ppf 0 0 in
    Fmt.repeat (List.length vars + 1) pp_col_dots ppf ()

  let min_width : type a. a Var.type' -> int =
  function Var.Nominal | Var.Ordinal _ -> 10 | _ -> 6

  let pp_vars ppf idx_width vars =
    let pp_var ppf (Var.V var) =
      Format.pp_set_tab ppf (); Fmt.string ppf var.name; Fmt.char ppf ' ';
      Fmt.repeat (min_width var.Var.type' - String.length var.name)
        Fmt.char ppf ' '
    in
    Format.pp_set_tab ppf (); Fmt.repeat (idx_width + idx_pad) Fmt.char ppf ' ';
    List.iter (pp_var ppf) vars; Fmt.tbreak ppf 0 0

  let pp_var_types ppf idx_width vars =
    let pp_var_type ppf (Var.V var) =
      Fmt.bold Var.Type.pp ppf var.Var.type'; Fmt.tbreak ppf 0 0;
    in
    Fmt.char ppf ' ' (* for the tab to move *); Fmt.tbreak ppf 0 0;
    List.iter (pp_var_type ppf) vars

  let pp_idx idx_width ppf i = Fmt.pf ppf "%-*d" idx_width i
  let pp_obs vars ppf idx_width i o =
    let pp_value o ppf (Var.V var) =
      Format.pp_open_box ppf 0;
      Var.pp_obs var ppf o;
      Format.pp_close_box ppf ();
      Fmt.tbreak ppf 0 0
    in
    Fmt.bold (pp_idx idx_width) ppf i; Fmt.tbreak ppf 0 0;
    List.iter (pp_value o ppf) vars

  let pp ?first ?last ?count ?vars:vs () ppf d =
    let vars = Option.value ~default:(vars d) vs in
    let first, count = range_spec ?first ?last ?count (length d) in
    let max = first + count - 1 in
    let idx_width = 1 + (Float.to_int @@ log10 (float (length d - 1))) in
    Format.pp_open_vbox ppf 0;
    pp_dim ppf idx_width d; Format.pp_print_cut ppf ();
    Format.pp_open_tbox ppf ();
    pp_vars ppf idx_width vars; pp_var_types ppf idx_width vars;
    (if first <> 0 then pp_var_dots ppf vars);
    for i = first to max do pp_obs vars ppf idx_width i d.os.(i) done;
    (if max <> length d - 1 then pp_var_dots ppf vars);
    Format.pp_close_tbox ppf ();
    Format.pp_close_box ppf ()

  let pp_top ppf d = pp ~count:10 () ppf d
  let show ?(ppf = Format.std_formatter) ?first ?last ?count ?vars d =
    pp ?first ?last ?count ?vars () ppf d

  (* Observation processing *)

  let sort ?take ?(stable = false) ~by d =
    let os = Array.copy d.os (* XXX avoid full copy in the future. *)in
    (if stable then Array.stable_sort by os else Array.sort by os);
    match take with
    | None -> { d with os }
    | Some 0 -> invalid_arg err_take
    | Some take ->
        let first, n = match take < 0 with
        | true -> Int.max 0 (Array.length os - take), -take
        | false -> 0, Int.min take (Array.length os)
        in
        { d with os = Array.sub os first n }

  let shuffle ~rand d = (* Fisher-Yates *)
    let os = Array.copy d.os in
    for i = length d - 1 downto 1 do
      let j = rand (i + 1) in
      let v = uget os i in
      uset os i (uget os j); uset os j v
    done;
    { d with os }

  let rev d =
    let os = Array.copy d.os and max = length d - 1 in
    for i = 0 to (length d / 2) - 1 do
      let oi = uget os i in
      let j = max - i in
      uset os i (uget os j); uset os j oi
    done;
    { d with os }

  let group_nil () = make [||]
  let group_obs = fun () ->
    let var = Var.any ~pp:pp_top "group" Fun.id in
    Obs.o1 var

  let group : type o. by:(o -> o -> int) -> o t -> o t t =
    fun ~by d ->
    let module O = struct type t = o let compare = by end in
    let module M = Map.Make (O) in
    let add_obs o m =
      let add = function
      | Some b -> Buffer.add b o; Some b
      | None -> let b = Buffer.make ~size:256 o in Buffer.add b o; Some b
      in
      M.update o add m
    in
    let acc = ref M.empty and max = length d - 1 in
    for i = 0 to max do acc := add_obs (uget d.os i) !acc done;
    let os = Array.make (M.cardinal !acc) (group_nil ()) in
    let set_group _ b i = uset os i { d with os = Buffer.to_array b }; i + 1 in
    ignore (M.fold set_group !acc 0);
    make ~obs:(group_obs ()) os

  let concat d =
    let len = fold (fun _ d acc -> acc + length d) d 0 in
    let b = Buffer.make ~size:len d.os.(0).os.(0) in
    for i = 0 to length d - 1 do Buffer.add_array b (uget d.os i).os done;
    { d.os.(0) with os = Buffer.to_array b }

  let append d0 d1 =
    let len0 = length d0 and len1 = length d1 in
    let r = Array.make (len0 + len1) d0.os.(0) in
    Array.blit d0.os 0 r 0 len0; Array.blit d1.os 0 r len0 len1;
    { d0 with os = r }

  let find_index ?(start = 0) p d =
    if start < 0 || start >= length d then None else
    let rec loop max i d =
      if i > max then None else
      if p i (uget d.os i) then Some i else loop max (i + 1) d
    in
    loop (length d - 1) start d

  let sub ?first ?last ?count d =
    match range_spec ?first ?last ?count (length d) with
    | _, 0 -> invalid_arg (err_range_spec ?first ?last ?count (length d))
    | first, n -> { d with os = Array.sub d.os first n }

  let filter p d = (* could avoid alloc on for all. *)
    let b = Buffer.make ~size:(length d) d.os.(0) in
    for i = 0 to length d - 1
    do let v = uget d.os i in if p i v then (Buffer.add b v) done;
    if Buffer.length b = 0 then None else Some { d with os = Buffer.to_array b }

  let err_sample n = Printf.sprintf "Cannot sample %d observations" n
  let sample ~rand n d =
    if n <= 0 then invalid_arg (err_sample n) else
    if n >= length d then d else
    let s = shuffle ~rand d in
    { s with os = Array.sub s.os 0 n }

  let witness_of (type o) ~group_by d =
    let module O = struct type t = o let compare = group_by end in
    let module M = Map.Make (O) in
    let acc = ref M.empty and max = length d - 1 in
    for i = 0 to max do
      let o = uget d.os (i) in
      acc := M.update o (function None -> Some o | _ as v -> v) !acc
    done;
    let add _ v acc = v :: acc in
    let os = Array.of_list (List.rev (M.fold add !acc [])) in
    { d with os }

  let distinct (type o) ?vars:vs d =
    let vars = Option.value ~default:(vars d) vs in
    Var.check_non_empty_order vars;
    let order = Var.unchecked_order (List.map (fun (Var.V v) -> Var.A v) vars)in
    let module O = struct type t = o let compare = order end in
    let module Set = Set.Make (O) in
    let acc = ref Set.empty in
    for i = 0 to length d - 1 do acc := Set.add (uget d.os i) !acc done;
    let os = Array.make (Set.cardinal !acc) d.os.(0) in
    let set os v i = uset os i v; i + 1 in
    ignore (Set.fold (set os) !acc 0);
    { d with os }

  (* Converting *)

  let to_array ?(borrow = false) d = if borrow then d.os else Array.copy d.os
  let of_array ?(lend = true) ?obs os =
  let os = if not lend then Array.copy os else os in make ?obs os

  let of_list ?obs l = make ?obs (Array.of_list l)
  let to_list d = Array.to_list d.os

  let of_t1 t l =
    let var = Var.v "v0" t Fun.id in
    var, make ~obs:(Obs.o1 var) (Array.of_list l)

  let of_t2 t0 t1 l =
    let var0 = Var.v "v0" t0 fst and var1 = Var.v "v1" t1 snd in
    (var0, var1), make ~obs:(Obs.o2 var0 var1) (Array.of_list l)

  module Csv = struct
    type fpath = string
    type var_names = string list option

   (* Decode

      A toy for now. Doesn't handle e.g. quoted strings.
      XXX use Buffer, and incrementalize. *)

    let rec decode_prod : type o a. (o, a) Var.Prod.t -> string list -> a =
    fun prod ss -> match prod with
    | Var.Prod.Unit f -> f
    | Var.Prod.Prod (prod, var) ->
        match ss with
        | [] -> failwith "missing column"
        | s :: ss ->
            let f = decode_prod prod ss in
            let v = try (Var.text_decoder var) s with
            | Failure e | Invalid_argument e ->
                failwith (Fmt.str "%s: %S: %s" (Var.name var) s e)
            in
            f v

    let decode ?var_names ?(quote = '\"') ?(sep = ',') ?(file = "-") obs s =
      let prod = Obs.prod obs in
      match String.split_on_char '\n' s with
      | [] -> Ok (None, None)
      | h :: vs as l ->
          (* Try parsing the first line, if it fails consider as a header.  *)
          let i, h, l =
            let ss = String.split_on_char sep h in
            match ignore (decode_prod prod ss) with
            | () -> 1, None, l
            | exception Failure _
            | exception Invalid_argument _ -> 2, Some ss, vs
          in
          let rec loop i acc = function
          | [] -> Ok (h, Some (rev (of_list ~obs acc)))
          | "" :: ls -> loop i acc ls
          | l :: ls ->
              match decode_prod prod (List.rev (String.split_on_char sep l))
              with
              | exception Failure e
              | exception Invalid_argument e ->
                  Error (Fmt.str "%s:%d:%s" file i e)
              | v -> loop (i + 1) (v :: acc) ls
          in
          loop i [] l


    let input ?var_names ?quote ?sep ?file obs ic =
      let s = In_channel.input_all ic in
      decode ?var_names ?quote ?sep ?file obs s

    let read ?var_names ?quote ?sep obs file =
      try
        In_channel.with_open_bin file @@ fun ic ->
        input ?var_names ?quote ?sep ~file obs ic
      with
      | Sys_error e -> Error e

    let get = function Error e -> invalid_arg e | Ok (_, d) -> Option.get d

    (* Encoding *)

    let encode_quoted b ~quote s =
      let len = String.length s in
      let max_idx = len - 1 in
      let flush b start i =
        if start < len then Stdlib.Buffer.add_substring b s start (i - start);
      in
      let rec loop start i =
        if i > max_idx then flush b start i else
        let next = i + 1 in
        if String.get s i <> quote then loop start next else
        begin
          flush b start i;
          Stdlib.Buffer.add_char b quote; Stdlib.Buffer.add_char b quote;
          loop next next
        end
      in
      Stdlib.Buffer.add_char b quote; loop 0 0; Stdlib.Buffer.add_char b quote

    let encode_string b ~quote ~sep s =
      try
        for i = 0 to String.length s - 1 do
          let c = String.get s i in
          if c = '\n' || c = quote || c = sep then raise Exit
        done;
        Stdlib.Buffer.add_string b s
      with
      | Exit -> encode_quoted b ~quote s

    let rec encode_prod_names :
      type o a. Stdlib.Buffer.t -> quote:Char.t -> sep:Char.t ->
      (o, a) Var.Prod.t -> bool
      =
      fun b ~quote ~sep prod -> match prod with
      | Var.Prod.Unit _ -> false
      | Var.Prod.Prod (prod, var) ->
          (if encode_prod_names b ~quote ~sep prod
           then Stdlib.Buffer.add_char b ',');
          encode_string b ~quote ~sep (Var.name var); true

    let rec encode_prod :
      type o a. Stdlib.Buffer.t -> quote:Char.t -> sep:Char.t ->
      (o, a) Var.Prod.t -> o -> bool
      =
      fun b ~quote ~sep prod o -> match prod with
      | Var.Prod.Unit _ -> false
      | Var.Prod.Prod (prod, var) ->
          (if encode_prod b ~quote ~sep prod o
           then Stdlib.Buffer.add_char b ',');
          encode_string b ~quote ~sep (Var.text_encoder var (Var.proj var o));
          true

    let encode ?(var_names = true) ?(quote = '\"') ?(sep = ',') d =
      let prod = Obs.prod (obs d) and b = Stdlib.Buffer.create 1024 in
      if var_names then begin
        ignore (encode_prod_names b ~quote ~sep prod);
        Stdlib.Buffer.add_char b '\n';
      end;
      for i = 0 to length d - 1 do
        ignore (encode_prod b ~quote ~sep prod (uget d.os i));
        Stdlib.Buffer.add_char b '\n';
      done;
      Stdlib.Buffer.contents b

    let output ?(var_names = true) ?(quote = '\"') ?(sep = ',') oc d =
      let[@inline] finish_line oc b =
        Stdlib.Buffer.add_char b '\n';
        Stdlib.Buffer.output_buffer oc b; Stdlib.Buffer.clear b;
      in
      let prod = Obs.prod (obs d) and b = Stdlib.Buffer.create 1024 in
      if var_names
      then (ignore (encode_prod_names b ~quote ~sep prod); finish_line oc b);
      for i = 0 to length d - 1 do
        ignore (encode_prod b ~quote ~sep prod (uget d.os i));
        finish_line oc b;
      done

    let write ?var_names ?quote ?sep file d =
      try
        Result.ok @@ Out_channel.with_open_bin file @@ fun oc ->
        output ?var_names ?quote ?sep oc d
      with
      | Sys_error e -> Error e

    type 'a schema = 'a Var.v list
    let schema_of_string ?(force = []) _ = failwith "TODO"
    let schema_of_file ?(force = []) _ = failwith "TODO"
    let schema_to_ocaml _ = failwith "TODO"
  end

  (* Toplevel *)

  module Top = struct
    let show = show
    let page d = failwith "TODO"
  end

  (* Variable queries *)

  type ('o, 'a) var = ('o, 'a) Var.t
  type 'o varv = 'o Var.v
  type 'a vtype = 'a Var.type'

  module Var = struct

    (* Summarize *)

    let count (type a) var d =
      let (module M : Map.S with type key = a) =
        Var.Type.value_map var.Var.type'
      in
      let upd = function None -> Some 1 | Some n -> Some (n + 1) in
      let acc = ref M.empty in
      for i = 0 to length d - 1
      do acc := M.update (var.Var.proj (uget d.os i)) upd !acc done;
      let os =
        let nil = var.Var.proj d.os.(0), 0 in
        let os = Array.make (M.cardinal !acc) nil in
        let set os k v i = uset os i (k, v); i + 1 in
        ignore (M.fold (set os) !acc 0); os
      in
      let fst, snd = Var.for_var_count var in
      let prod = Var.Prod.(unit (fun fst snd -> (fst, snd)) * fst * snd) in
      let obs = Obs.v prod in
      make ~obs os

    let fsum var d = (* See §3 https://doi.org/10.1007/s00607-005-0139-x  *)
      let sum = ref (var.Var.proj (uget d.os 0)) in
      let c = ref 0. in
      let count = ref 1 in
      for i = 1 to length d - 1 do
        let v = var.Var.proj (uget d.os i) in
        if Float.is_nan v then () else begin
          let t = !sum +. v in
          if Float.abs !sum >= Float.abs v
          then (c := !c +. (!sum -. t) +. v)
          else (c := !c +. (v -. t) +. !sum);
          sum := t;
          incr count;
        end
      done;
      !sum +. !c, !count

    let isum var d =
      let sum = ref 0 and max = length d - 1 in
      for i = 0 to max do sum := !sum + (var.Var.proj (uget d.os i)) done; !sum

    let bsum var d =
      let sum = ref 0 and max = length d - 1 in
      for i = 0 to max do if var.Var.proj (uget d.os i) then incr sum done; !sum

    let sum : type a. ('o, a) Var.t -> 'o t -> float =
    fun var d -> match var.Var.type' with
    | Var.Float -> fst (fsum var d)
    | Var.Int -> Float.of_int (isum var d)
    | Var.Bool -> Float.of_int (bsum var d)
    | Var.Any | Var.Nominal | Var.Ordinal _ -> Float.nan

    let mean : type a. ('o, a) Var.t -> 'o t -> float =
    fun var d -> match var.Var.type' with
    | Var.Float -> let sum, count = fsum var d in sum /. float count
    | Var.Int -> float (isum var d) /. float (length d)
    | Var.Bool -> float (bsum var d) /. float (length d)
    | Var.Any | Var.Nominal | Var.Ordinal _ -> Float.nan

    let fquantile var d =
      let os = Array.copy d.os in
      Array.stable_sort (Var.compare_obs var) os;
      (* If there are nans they are now at the beginning, skip them. *)
      let zero =
        let rec loop max i os =
          if i > max || not (Float.is_nan (var.Var.proj os.(i))) then i else
          loop max (i + 1) os
        in
        loop (Array.length os - 1) 0 os
      in
      if zero = Array.length os then (* only nans *) Fun.const nan else
      let n = Array.length os - zero in
      fun p ->
        if p <= 0. then var.Var.proj os.(zero) else
        if p >= 1. then var.Var.proj os.(n - 1) else
        let h = (float (n - 1)) *. p in
        let hf = Float.floor h in
        let i = Float.to_int h in
        let v0 = var.Var.proj (Array.get os (zero + i)) in
        (* N.B. v1 is in bounds because p < 1. *)
        let v1 = var.Var.proj (Array.get os (zero + i + 1)) in
        v0 +. (h -. hf) *. (v1 -. v0)

    let gquantile vfloat var d =
      let os = Array.copy d.os in
      Array.stable_sort (Var.compare_obs var) os;
      fun p ->
        if p <= 0. then vfloat @@ var.Var.proj os.(0) else
        if p >= 1. then vfloat @@ var.Var.proj os.(Array.length os - 1) else
        let h = (float (Array.length os - 1)) *. p in
        let hf = Float.floor h in
        let i = Float.to_int h in
        let v0 = vfloat @@ var.Var.proj (Array.get os i) in
        let v1 = var.Var.proj (Array.get os (i + 1)) in
        v0 +. (h -. hf) *. ((vfloat v1) -. v0)

    let quantile : type o a. (o, a) Var.t -> o t -> (float -> float) =
    fun var d -> match var.Var.type' with
    | Var.Float -> fquantile var d
    | Var.Int -> gquantile float var d
    | Var.Bool -> gquantile (fun b -> if b then 1. else 0.) var d
    | Var.Any | Var.Nominal | Var.Ordinal _ -> Fun.const Float.nan

    let median var d = quantile var d 0.5

    let gvariance get d =
      (* For the algo see the TAOCP vol. 2. *)
      let m = ref 0. in
      let s = ref 0. in
      let count = ref 0. in
      for i = 0 to length d - 1 do
        let v = get (uget d.os i) in
        if Float.is_nan v then () else begin
          let delta = v -. !m in
          count := !count +. 1.0;
          m := !m +. (delta /. !count);
          s := !s +. (delta *. (v -. !m));
        end
      done;
      (* XXX we return mean with it ? *)
      if !count < 2. then nan else !s /. (!count -. 1.)

    let variance : type o a. (o, a) Var.t -> o t -> float =
    fun var d -> match var.Var.type' with
    | Var.Float -> gvariance var.Var.proj d
    | Var.Int -> gvariance (fun o -> float (var.Var.proj o)) d
    | Var.Bool -> gvariance (fun o -> if (var.Var.proj o) then 1. else 0.) d
    | Var.Any | Var.Nominal | Var.Ordinal _ -> Float.nan

    let deviation var d = sqrt (variance var d)

    (* Group *)

    let group_obs var =
      let k = Var.with_proj var fst in
      let v = Var.any ~pp:pp_top "group" snd in
      Obs.o2 k v

    let group : type o a. by:(o, a) Var.t -> o t -> (a * o t) t =
    fun ~by d ->
    (* We could do the map on variable values, see how all this fares with
       Gprod. *)
    let module O = struct type t = o let compare = Var.compare_obs by end in
    let module M = Map.Make (O) in
    let add_obs o m =
      let add = function
      | Some b -> Buffer.add b o; Some b
      | None -> let b = Buffer.make ~size:256 o in Buffer.add b o; Some b
      in
      M.update o add m
    in
    let acc = ref M.empty and max = length d - 1 in
    for i = 0 to max do acc := add_obs (uget d.os i) !acc done;
    let os =
      Array.make (M.cardinal !acc)
        ((by.Var.proj (fst (M.min_binding !acc))), make [||])
    in
    let set_group k b i =
      uset os i (by.Var.proj k, { d with os = Buffer.to_array b }); i + 1
    in
    ignore (M.fold set_group !acc 0);
    make ~obs:(group_obs by) os

    (* Range *)

    let fmin var d =
      let min = ref (var.Var.proj d.os.(0)) in
      for i = 1 to length d - 1
      do min := Float.min_num !min (var.Var.proj (uget d.os i)) done;
      !min

    let gmin var d =
      let compare = Var.Type.value_compare var.Var.type' in
      let min = ref (var.Var.proj d.os.(0)) in
      for i = 1 to length d - 1 do
        let v = var.Var.proj (uget d.os i) in
        if compare !min v > 0 then min := v;
      done;
      !min

    let min : type a. ('o, a) Var.t -> 'o t -> a = fun var d ->
      match var.Var.type' with Var.Float -> fmin var d | kind -> gmin var d

    let fmax var d =
      let max = ref (var.Var.proj d.os.(0)) in
      for i = 1 to length d - 1 do
        let v = var.Var.proj (uget d.os i) in
        max := Float.max_num !max v;
      done;
      !max

    let gmax var d =
      let compare = Var.Type.value_compare var.Var.type' in
      let max = ref (var.Var.proj d.os.(0)) in
      for i = 1 to length d - 1 do
        let v = var.Var.proj (uget d.os i) in
        if compare !max v < 0 then max := v;
      done;
      !max

    let max : type a. ('o, a) Var.t -> 'o t -> a = fun var d ->
      match var.Var.type' with Var.Float -> fmax var d | kind -> gmax var d

    let fmin_max var d =
      let zero = var.Var.proj d.os.(0) in
      let min = ref zero in
      let max = ref zero in
      for i = 1 to length d - 1 do
        let v = var.Var.proj (uget d.os i) in
        min := Float.min_num !min v;
        max := Float.max_num !max v;
      done;
      !min, !max

    let gmin_max var d =
      let compare = Var.Type.value_compare var.Var.type' in
      let zero = var.Var.proj d.os.(0) in
      let min = ref zero in
      let max = ref zero in
      for i = 1 to length d - 1 do
        let v = var.Var.proj (uget d.os i) in
        if compare !min v > 0 then min := v;
        if compare !max v < 0 then max := v;
      done;
      !min, !max

    let min_max : type a. ('o, a) Var.t -> 'o t -> a * a = fun var d ->
      match var.Var.type' with
      | Var.Float -> fmin_max var d | kind -> gmin_max var d

    let values (type a) var d =
      let (module Set : Set.S with type elt = a) =
        Var.Type.value_set var.Var.type'
      in
      let acc = ref Set.empty and max = length d - 1 in
      for i = 0 to max do acc := Set.add (var.Var.proj (uget d.os i)) !acc done;
      let os =
        let os = Array.make (Set.cardinal !acc) (var.Var.proj d.os.(0)) in
        let set os v i = uset os i v; i + 1 in
        ignore (Set.fold (set os) !acc 0); os
      in
      let obs = Obs.o1 (Var.for_var_values var) in
      make ~obs os

    let dom :
      type set a. (module Set.S with type t = set and type elt = a) ->
      ('o, a) Var.t -> 'o t -> set
      =
      fun (module Set) var d ->
      let acc = ref Set.empty and max = length d - 1 in
      for i = 0 to max do acc := Set.add (var.Var.proj (uget d.os i)) !acc done;
      !acc

    (* Transform *)

    let update var upd d =
      let zero = uget d.os 0 in
      let os = Array.make (length d) (Obs.set d.obs var (upd 0 zero) zero) in
      for i = 1 to length d - 1 do
        let o = uget d.os i in uset os i (Obs.set d.obs var (upd i o) o)
      done;
      make ~obs:(obs d) os

    let set var v i d =
      if i < 0 || i >= length d
      then invalid_arg (err_bounds i 0 (length d - 1)) else
      let o' = Obs.set d.obs var v (uget d.os i) in
      let os = Array.copy d.os in
      uset os i o'; { d with os }

    let cumsum_obs = Obs.o1 Var.cumsum

    let fcumsum var d =
      let os = Array.make (length d) (var.Var.proj (d.os.(0))) in
      let sum = ref os.(0) in
      for i = 1 to length d - 1
      do sum := !sum +. (var.Var.proj (uget d.os i)); uset os i !sum done;
      make ~obs:cumsum_obs os

    let icumsum var d =
      let os = Array.make (length d) (float (var.Var.proj (d.os.(0)))) in
      let sum = ref os.(0) in
      for i = 1 to length d - 1
      do sum := !sum +. (float (var.Var.proj (uget d.os i))); uset os i !sum
      done;
      make ~obs:cumsum_obs os

    let bcumsum var d =
      let b = function true -> 1. | false -> 0. in
      let os = Array.make (length d) (b (var.Var.proj (d.os.(0)))) in
      let sum = ref os.(0) in
      for i = 1 to length d - 1
      do sum := !sum +. (b (var.Var.proj (uget d.os i))); uset os i !sum
      done;
      make ~obs:cumsum_obs os

    let cumsum : type a. ('o, a) Var.t -> 'o t -> float t =
    fun var d -> match var.Var.type' with
    | Var.Float -> fcumsum var d
    | Var.Int -> icumsum var d
    | Var.Bool -> bcumsum var d
    | Var.Any | Var.Nominal | Var.Ordinal _ ->
        let os = Array.make (length d) Float.nan in
        make ~obs:cumsum_obs os
  end
end

module O = struct
  let v var = var.Var.proj
  let c = Fun.const

  let is_nan v o = Float.is_nan (v o)
  let between min max var o = Var.between (min o) (max o) var o

  let ( = )  v0 v1 o = v0 o = v1 o
  let ( <> ) v0 v1 o = v0 o <> v1 o
  let ( < )  v0 v1 o = v0 o < v1 o
  let ( > )  v0 v1 o = v0 o > v1 o
  let ( <= ) v0 v1 o = v0 o <= v1 o
  let ( >= ) v0 v1 o = v0 o >= v1 o
  let ( || ) v0 v1 o = v0 o || v1 o
  let ( && ) v0 v1 o = v0 o && v1 o

  let ( + ) v0 v1 o = v0 o + v1 o
  let ( - ) v0 v1 o = v0 o - v1 o
  let ( * ) v0 v1 o = v0 o * v1 o
  let ( / ) v0 v1 o = v0 o / v1 o

  let ( +. ) v0 v1 o = v0 o +. v1 o
  let ( -. ) v0 v1 o = v0 o -. v1 o
  let ( *. ) v0 v1 o = v0 o *. v1 o
  let ( /. ) v0 v1 o = v0 o /. v1 o
end

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The evidence programmers

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
