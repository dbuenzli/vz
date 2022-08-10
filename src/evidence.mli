(*---------------------------------------------------------------------------
   Copyright (c) 2022 The evidence programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Rectangular datasets.

    [Evidence] provides tools to describe and manipulate rectangular
    in-memory datasets.

    A {e dataset} is a sequence of {e observations} (rows) represented
    by a sequence of OCaml values of a given type. Each observation
    can be decomposed into a set of {e variables} (columns) via
    arbitrary projection functions.

    This dataset organisation satisifies the principle of
    {{:https://doi.org/10.18637/jss.v059.i10}tidy data}.

    See the {{!todo}TODO}. *)

(**/**)
type 'a fmt = Format.formatter -> 'a -> unit
(** @canonical Evidence.Var.fmt *)
(**/**)

(** Observation variables.

    This module provides a type to describe and type observation
    variables. Essentially, an observation variable is a named
    projection function. *)
module Var : sig

  (** {1:vartypes Variable types} *)

  type 'a type' =
  | Bool : bool type'
  | Int : int type'
  | Float : float type'
  | Ordinal : ('a -> 'a -> int) -> 'a type'
  | Nominal : string type'
  | Any : 'a type' (** Uses {!Stdlib.compare} to order values. *)
  (** The type for types of variables. *)

  (** Variable types. *)
  module Type : sig

    (** {1:types Types} *)

    type 'a t = 'a type'
    (** The type for type of variables. *)

    val is_numeric : 'a t -> bool
    (** [is_numeric t] is [true] if [t] is [Bool], [Int] or [Float]. *)

    val pp : 'a type' fmt
    (** [pp ppf t] formats type [t] on [ppf]. *)

    (** {1:ops Operations on the values of types} *)

    val value_compare : 'a type' -> ('a -> 'a -> int)
    (** [value_compare t] is the order underlying the values of type [t]. *)

    val value_pp : 'a type' -> 'a fmt
    (** [value_pp t] is a formatter for values of type [t]. *)

    val value_set : 'a type' -> (module Set.S with type elt = 'a)
    (** [value_set t] is a set for values of type [t]. *)

    val value_map : 'a type' -> (module Map.S with type key = 'a)
    (** [value_map t] is a map for values of type [t]. *)
  end

(*
  (** {1:conv Converters} *)

  (** Variable value converters.

      Packs a textual codec and a formatter. *)
  module Conv : sig
    type 'a t
    val bool : bool t
    val int : int t
    val float : float t
  end
*)

  (** {1:vars Variables} *)

  type name = string
  (** The type for variable names. *)

  type ('o, 'a) t
  (** The type for describing variables of type ['a] for an observation
      of type ['o]. *)

  type 'o v = V : ('o, 'a) t -> 'o v (** *)
  (** The type for existential variables for observations of type ['o]. *)

  val v :
    ?doc:string -> ?label:string -> ?pp:'a fmt -> name -> 'a type' ->
    ('o -> 'a) -> ('o, 'a) t
  (** [v ?doc ?pp name type' proj] an observation variable with

      {ul
      {- [name] the name or identifier of the variable.}
      {- [type'] the type of the variable.}
      {- [proj] the projection of the variable.}
      {- [pp] a formatter for the variable, defaults to {!Type.value_pp}}
      {- [label] a label for the variable used in graphical rendering}
      {- [doc] a documentation string for the variable}}

      {b Warning.} For [float] types, if you want [nan] values to be
      treated by {!Dataset} as missing values, you need to use [Float]
      and not [Any] or [Ordinal].

      {b Note.} Variables have a notion of identity, if you create two
      variables with the same parameters they will not be deemed equal
      by {!equal}. *)

  (** {2:convenience Convenience constructors} *)

  val bool :
    ?doc:string -> ?label:string -> ?pp:bool fmt -> name -> ('o -> bool) ->
    ('o, bool) t
  (** [bool name proj] is [v name Bool proj]. *)

  val int :
    ?doc:string -> ?label:string -> ?pp:int fmt -> name -> ('o -> int) ->
    ('o, int) t
  (** [int name proj] is [v name Int proj]. *)

  val float :
    ?doc:string -> ?label:string -> ?pp:float fmt -> name -> ('o -> float) ->
    ('o, float) t
  (** [float name proj] is [v name Float proj]. *)

  val nominal :
    ?doc:string -> ?label:string -> ?pp:string fmt -> name -> ('o -> string) ->
    ('o, string) t
  (** [nominal name proj] is [v name Nominal]. *)

  val ordinal :
    ?doc:string -> ?label:string -> ?pp:'a fmt -> name -> ('a -> 'a -> int) ->
    ('o -> 'a) -> ('o, 'a) t
  (** [ordinal name compare proj] is [v name (Ordinal compare) proj]. *)

  val any :
    ?doc:string -> ?label:string -> ?pp:'a fmt -> name -> ('o -> 'a) ->
    ('o, 'a) t
  (** [any name proj] is [v name Any proj]. *)

  val const :
    ?doc:string -> ?label:string -> ?pp:'a fmt -> name -> 'a type' -> 'a ->
    ('o, 'a) t
  (** [const name type' v] is [v name type' (Fun.const v)]. *)

  val id : 'a type' -> ('a, 'a) t
  (** [id type'] is an [v "id" type' Fun.id] identity variable. *)

  (** {1:properties Properties} *)

  val doc : ('o, 'a) t -> string
  (** [doc var] is the documentation string of [var]. *)

  val exist : ('o, 'a) t -> 'o v
  (** [exist var] is an existential for [var]. *)

  val name : ('o, 'a) t -> name
  (** [name var] is the name of [var]. *)

  val label : ('o, 'a) t -> string
  (** [label var] is the label of [var]. *)

  val pp : ('o, 'a) t -> 'a fmt
  (** [pp var] is the formatter of [var]. *)

  val proj : ('o, 'a) t -> ('o -> 'a)
  (** [proj var] is the projection of [var]. *)

  val type' : ('o, 'a) t -> 'a type'
  (** [type' var] is the type of [var]. *)

  (** {1:transforming Transforming} *)

  val alter :
    ?doc:string -> ?label:string -> ?pp:'a fmt -> ?name:name -> ('o, 'a) t ->
    ('o, 'a) t
   (** [alter ?doc ?label ?name ?pp var] is [var] with the given parameters. The
       resulting variable is equal to [var]. *)

  val map :
    ?doc:string -> ?label:string -> ?pp:'b fmt ->
    ('a -> 'b) -> ('o, 'a) t -> name -> 'b type' -> ('o, 'b) t
  (** [map f var name type'] is a new variable derived via [f] from [var]. *)

(*
  val merge : ('a -> 'b -> 'c) -> ('o, 'a) t -> ('o, 'b) t -> ('o, 'c) t
  val split : ('a -> 'b * 'c) -> ('o, 'a) t -> ('o, 'b) t * ('o, 'c) t
*)

  (** {1:value_ops Operations on variable values} *)

  val equal_value : ('o, 'a) t -> ('a -> 'a -> bool)
  (** [equal_value var] is a comparison function for the values of [var].
      [nan] values are deemed equal. *)

  val compare_value : ('o, 'a) t -> ('a -> 'a -> int)
  (** [compare_value var] is a comparison function for values of [var].
      [nan] values are deemed equal and lower than any other float value. *)

  val min_value : ('o, 'a) t -> ('a -> 'a -> 'a)
  (** [min_value var] is a function that determines the minimum for
      values of [var]. On [Float] variables this uses {!Float.min_num}
      which treats [nan]s as missing values. *)

  val max_value : ('o, 'a) t -> ('a -> 'a -> 'a)
  (** [min_value var] is a function that determines the maximum for
      values of [var]. On [Float] variables this uses {!Float.max_num}
      which treats [nan]s as missing values. *)

  val pp_value : ('o, 'a) t -> 'a fmt
  (** [pp_value var] is a formatter for the values of [var]. *)

  (** {1:obs_ops Operations on observations}

      See also {!Obs}. *)

  type 'o order =
  | A : ('o, 'a) t -> 'o order (** Ascending *)
  | D : ('o, 'a) t -> 'o order (** Descending *)
  (** The type for variable order. *)

  val order : 'o order list -> ('o -> 'o -> int)
  (** [order vars] is the lexicographic ascending or descending order of
      variables [vars] of observations. Raises [Invalid_argument] if
      [vars] is empty. *)

  val is_na : ('o, float) t -> ('o -> bool)
  (** [is_na var o] is [Float.is_nan (proj var o)]. *)

  val between : 'a -> 'a -> ('o, 'a) t -> ('o -> bool)
  (** [between min max var o] is [true] iff [proj var o] is
      in the range \[[min];[max]\]. This uses {!compare_value}. *)

  val equal_obs : ('o, 'a) t -> ('o -> 'o -> bool)
  (** [equal_obs var] tests for equality on variable [var]. [nan] values
      are deemed equal. *)

  val compare_obs : ('o, 'a) t -> ('o -> 'o -> int)
  (** [compare_obs var] tests for equality on variable [var]. [nan] values
      are deemed equal. *)

  val min_obs : ('o, 'a) t -> 'o -> 'o -> 'o
  (** [min_obs var o0 o1] is the observation with the minimal [var]
      value (either is returned on ties). On [Float] variables this uses
      {!Float.min_num} which treats [nan]s as missing values. *)

  val max_obs : ('o, 'a) t -> 'o -> 'o -> 'o
  (** [max_obs var o0 o1] is the observation with the maximal [var]
      value (either is returned on ties). On [Float] variables this uses
      {!Float.max_num} which treats [nan]s as missing values. *)

  val pp_obs : ('o, 'a) t -> 'o fmt
  (** [pp_obs var] is a formatter for the variable [var] of observations. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : ('o, 'a) t -> ('p, 'b) t -> bool
  (** [equal var0 var1] is true iff [var0] and [var1] is the same variable. *)

  val compare : ('o, 'a) t -> ('p, 'b) t -> int
  (** [compare] is a total order on variables compatible with {!equal}. *)

  (** {1:fmt Formatting} *)

  type 'a fmt = Format.formatter -> 'a -> unit
  (** The type for formatters of values of type ['a]. *)

  val pp : ('o, 'a) t fmt
  (** [pp] formats a variable's name and type. *)

  (** {1:std Standard variables} *)

  val for_var_count : ('o, 'a) t -> ('a * int, 'a) t * ('a * int, int) t
  (** [for_var_count var] are two variables to access the result of a
      {!Dataset.Var.count} dataset. *)

  val for_var_values : ('o, 'a) t -> ('a, 'a) t
  val for_var : ('o, 'a) t -> ('a, 'a) t

  val cumsum : (float, float) t
  (** [cumsum] is the variable for {!Data.Var.cum_sum} results. *)

  (** {1:prod Product} *)

  (** Variable products. *)
  module Prod : sig
    type ('o, 'p) var = ('o, 'p) t
    (** The type for variables. *)

    type ('o, 'p) t =
    | Unit : 'p -> ('o, 'p) t
    | Prod : ('o, 'a -> 'b) t * ('o, 'a) var -> ('o, 'b) t (** *)
    (** The type for contructing a cartesian product with function ['p]
        whose final result is represented by OCaml values of type ['o]. *)

    val unit : 'a -> ('o, 'a) t
    (** [unit f] is a (virtual) unit variable with contructor [f] to be
        saturated by {!prod}. *)

    val prod : ('o, 'a -> 'b) t -> ('o, 'a) var -> ('o, 'b) t
    (** [prod p var] is the product of variables [p] with [var]. *)

    val ( * ) : ('o, 'a -> 'b) t -> ('o, 'a) var -> ('o, 'b) t
    (** [vs * var] is [prod vs var]. *)

    val absurd : unit -> ('o, 'a -> 'b) t
    (** [absurd ()] is an absurd {!unit} to use to construct
        a product that cannot construct observations – trying
        raises [Invalid_argument]. *)

    val vars : ('o, 'o) t -> 'o v list
    (** [vars p] are the variables in product [p]. *)
  end

  (** Generalize {!Prod}.

      This could be useful for group by on multiple variables. *)
  module Gprod : sig
    type ('o, 'p) var = ('o, 'p) t
    (** The type for variables. *)

    type ('c, 'o, 'p) t =
    | Unit : 'p -> ('c, 'o, 'p) t
    | Prod : ('c, 'o, 'a -> 'b) t * ('o, 'a) var -> ('c, 'o, 'b) t (** *)
    (** The type for contructing a cartesian product ['c] with function ['p]
        by projecting values from ['o]. *)

    val unit : 'a -> ('c, 'o, 'a) t
    (** [unit f] is a (virtual) unit variable with contructor [f] to be
        saturated by {!prod}. *)

    val prod : ('c, 'o, 'a -> 'b) t -> ('o, 'a) var -> ('c, 'o, 'b) t
    val ( * ) : ('c, 'o, 'a -> 'b) t -> ('o, 'a) var -> ('c, 'o, 'b) t

    val proj : ('c, 'o, 'c) t -> 'o -> 'c

    (* Now that one should be possible:
       val order_obs : ('c, 'o, 'c) t -> ('o -> 'o -> int)

       But can't be done because we don't have the projectors
       for [c] note however that for group_by we use [order_obs]
       (but we could use the variable.

       val order : ('c, 'o, 'c) t -> ('c -> 'c -> int)

      val as_var : ('c, 'o, 'c) t -> name -> ('o, 'c) var *)
  end

  (*
  (** {1:maps Maps} *)

  (** Heterogenous variable maps. *)
  module Map : sig

    type key = K : ('o, 'a) t -> key
    type binding = B : ('o, 'a) t * 'a -> binding
  (** The type for variable binding value. *)


    type t

  end*)
end

(** Observations.

    This module provides a type to describe observations. Essentially
    an observation is an arbitrary OCaml type and its decomposition
    into variables which we represent by a variable product.

    Using a variable product rather than a list of exisentials allows
    to construct observations from its variables. If for some reason
    you can't do that use {!Var.Prod.absurd}, note that by doing so
    functions that need to construct observations like
    {!Dataset.Var.update} will
    raise [Invalid_argument]. *)
module Obs : sig

  (** {1:obs Observations} *)

  type 'o t
  (** The type for describing observations of type ['o]. *)

  val v : ?doc:string -> ('o, 'o) Var.Prod.t -> 'o t
  (** [v ?doc vars] are observations described by the variables product
      [vars]. [doc] is a documentation string. *)

  val empty : unit -> 'o t
  (** [empty ()] observes the void. *)

  val doc : 'o t -> string
  (** [doc obs] is the documentation string of [obs]. *)

  val prod : 'o t -> ('o, 'o) Var.Prod.t
  (** [prod obs] is the product of variables describing [obs]. *)

  val vars : 'o t -> 'o Var.v list
  (** [vars obs] is are the variables of observations [obs]. *)

  val compare : 'o t -> 'o -> 'o -> int
  (** [compare obs] is [Var.order (vars obs)], orders observations
      in the lexicographic order of their variables. *)

  val set : 'o t -> ('o, 'a) Var.t -> 'a -> 'o -> 'o
  (** [set obs var v o] sets variable [var] to [v] in [o]. Raises
      [Invalid_argument] if the product is absurd and has no effect if
      [var] is not part of [prod obs]. *)

  val o1 : ?doc:string -> ('a, 'a) Var.t -> 'a t
  val o2 :
    ?doc:string -> ('a * 'b, 'a) Var.t -> ('a * 'b, 'b) Var.t -> ('a * 'b) t
end

(** Observation sequences.

    A dataset is an non-empty, immutable, indexed sequence of observations. *)
module Dataset : sig

  (** {1:dataset Datasets} *)

  type rand = int -> int
  (** The type for randomness in ranges \[[0];[n-1]\]. The function must
      be such that [rand n] is a uniformly distributed random number in
      the range \[[0];[n-1]\]. *)

  type count = int
  (** The type for counts. A count [n] must not be zero. If [n] is positive
      it represents the first [n] observations. If [n] is negative it represents
      the last [n] observations. *)

  type 'o t
  (** The type for datasets with observations of type ['o]. *)

  val init : ?obs:'o Obs.t -> int -> (int -> 'o) -> 'o t
  (** [init ?doc n obs] initializes a dataset with [n] observations
      drawn from [obs]. [obs] is used to derive formatters,
      serializers and for certain function defaults. Raises
      [Invalid_argument] if [n < 1]. *)

  val reobs : 'o Obs.t -> 'o t -> 'o t
  (** [reobs obs d] observes [d] with [obs]. *)

  (** {1:props Properties} *)

  val length : 'o t -> int
  (** [length d] is the (positive) number of observations in [d]. *)

  val obs : 'o t -> 'o Obs.t
  (** [obs d] is the observation description of [d]. *)

  val vars : 'o t -> 'o Var.v list
  (** [vars d] is [Obs.vars (objs d)], the known variables of [d]. *)

  val get : int -> 'o t -> 'o
  (** [get i d] is the zero-based [i]th observation of [d]. Raises
      [Invalid_argument] if [i] is not in \[[0];[length d - 1]\]. *)

  (** {1:traverse Traversing} *)

  val fold : (int -> 'o -> 'a -> 'a) -> 'o t -> 'a -> 'a
  (** [fold f d acc] folds [f] over [d] starting with [acc]. *)

  val iter : (int -> 'o -> unit) -> 'o t -> unit
  (** [iter f d] iterates [f] overs [d]. *)

  (** {1:transform Transforming} *)

  val map : ?obs:'p Obs.t -> (int -> 'o -> 'p) -> 'o t -> 'p t
  (** [map ~obs f d] maps observations of [d] to [obs] observations
      with [f]. [obs] defaults to {!Obs.empty}. *)

  val filter_map :
    ?obs:'p Obs.t -> (int -> 'o -> 'p option) -> 'o t -> 'p t option
  (** [filter_map] is like {!map} but drops [None] maps.  [obs]
      defaults to {!Obs.empty}. *)

  val update : (int -> 'o -> 'o) -> 'o t -> 'o t
  (** [update f d] updates each observation [o] of [d] with [f]. *)

  val set : int -> 'o -> 'o t -> 'o t
  (** [set i o d] is a new dataset with the [i]th observation of [d]
      set to [o]. For efficiency do not call that function
      repeateadly, {!update} is a better option. *)

  (** {1:grouping Grouping} *)

  val append : 'o t -> 'o t -> 'o t
  (** [append d0 d1] appends the observations of [d1] to those of [d0].
      The {!val-obs} of [d0] is used for the result. *)

  val concat : 'o t t -> 'o t
  (** [concat ds] concatenates the observations of the groups [ds].
      The {!val-obs} of the first element is used for the result. *)

  val group : by:('o -> 'o -> int) -> 'o t -> 'o t t
  (** [group ~by d] groups observations of [d] by the equivalence
      relation determined by [by]. The sequence of groups is ordered
      by [by]. See also {!Dataset.Var.group}. *)

  (** {1:ordering Ordering} *)

  val sort : ?take:count -> ?stable:bool -> by:('o -> 'o -> int) -> 'o t -> 'o t
  (** [sort ~by d] sorts observations of [d] using [~by]. If [stable]
      is true (defaults to [false]), the order of [d] is kept on
      ties. If [take] is specified only returns [take] first ([> 0]) or
      last ([< 0]) observations from the sort. *)

  val shuffle : rand:rand -> 'o t -> 'o t
  (** [shuffle ~rand d] randomly permutes the observations [d] using [rand]
      for randomness. *)

  val rev : 'o t -> 'o t
  (** [rev o] reverses the order of observations. *)

  (** {1:extracting Extracting} *)

  val find_index : ?start:int -> (int -> 'o -> bool) -> 'o t -> int option
  (** [find_index ~start p d] is is the smallest index [i], if any, such
      that [p i (get d i)] is [true]. [start] defaults to [0] and
      [None] is returned if it is out of bounds. *)

  val sub : ?first:int -> ?last:int -> ?count:count -> 'o t -> 'o t
  (** [sub ~first ~last ~count d] are the observations whose indices
      exist in the range \[[first];[last]\]. [first] defaults to [0]
      and [last] to [length d - 1]. If [count] is specified only
      returns the [count] first ([count > 0]) or last ([count < 0])
      elements of the range.

      Raises [Invalid_argument] if the result is empty, that is if
      there is no observations in the range for [d] or [count = 0]. *)

  val filter : (int -> 'o -> bool) -> 'o t -> 'o t option
  (** [filter p d] are the observations of [d] that satisfy [p] or
      [None] if none do. *)

  val sample : rand:rand -> int -> 'o t -> 'o t
  (** [sample ~rand n d] samples [n] observations from [d] using
      [rand] as a random source. Raises [Invalid_argument] if
      [n < 1]. *)

  val witness_of : group_by:('o -> 'o -> int) -> 'o t -> 'o t
  (** [witness_of ~group_by] groups by [group_by] and for each group
      keeps the lowest index observation as the group
      representative. The result is ordered by [group_by]. *)

  val distinct : ?vars:'o Var.v list -> 'o t -> 'o t
  (** [distinct d ~vars] is [witness_of ~order_by:(Var.order vars)].
      These are the observations that have distinct [vars] variables
      (defaults to {!vars}). Keeps the observation with the lowest
      index as the representative. The result is ordered in the
      lexicographic order of vars.

      Raises [Invalid_argument] if [vars] is empty. *)

  (** {1:query Variable processing} *)

  (**/**)
  type ('o, 'a) var = ('o, 'a) Var.t
  type 'o varv = 'o Var.v
  type 'a vtype = 'a Var.type'
  (**/**)

  (** Variable processing.

      {b Note.} Unless otherwise specified {!Var.Float} variables
      treat [nan]s are missing values. [nan] values can still be returned,
      for example if that's the single value available. *)
  module Var : sig

    (** {1:summarize Summarize} *)

    val count : ('o, 'a) Var.t -> 'o t -> ('a * int) t
    (** [count var d] groups observations by unique values of variable
        [var] and reports the number of observations found in each group.
        Variables values are sorted in increasing order, the dataset
        can be accessed with {!Var.for_var_count}[ var]. See also
        {!group}. *)

    val sum : ('o, 'a) Var.t -> 'o t -> float
    (** [sum var d] is the sum of the values of [var] in [d]. [nan]
        values are excluded from the computation. This is
        [nan] on {{!Evidence.Var.Type.is_numeric}non-numeric}
        types or if there are only [nan]s. On floats uses the
        {{:https://doi.org/10.1007/s00607-005-0139-x} Kahan-Babuška
        algorithm} (§3). *)

    val mean : ('o, 'a) Var.t -> 'o t -> float
    (** [mean var d] is the arithmetic mean of variable [var] in
        [d]. [nan] values are excluded from the computation. This is
        [nan] on {{!Evidence.Var.Type.is_numeric}non-numeric}
        types or if there are only [nan]s. Uses {!sum} to compute the
        result. *)

    val quantile : ('o, 'a) Var.t -> 'o t -> (float -> float)
    (** [quantile var d] is a function [quant] such that [quant p] is
        the [p]-quantile of [d] on variable [var] using the
        {{:https://en.wikipedia.org/wiki/Quantile#Estimating_quantiles_from_a_sample}
        R-7 definition}. [quant] clamps its argument to \[[0];[1]\]. [nan]
        values are excluded from the computation. The function is
        [Fun.const nan] on {{!Evidence.Var.Type.is_numeric}non-numeric} types
        or if there only [nan]s. *)

    val median : ('o, 'a) Var.t -> 'o t -> float
    (** [median var d] is [quantile var d 0.5]. If you also need
        other quantiles, use the function returned by [quantile var d]. *)

    val variance : ('o, 'a) Var.t -> 'o t -> float
    (** [variance var d] is the {e unbiased}
     {{:http://mathworld.wolfram.com/SampleVariance.html}sample
        variance} of variable [var] in [d] computed using
        {{:https://www.tandfonline.com/doi/abs/10.1080/00401706.1962.10490022}
        Welford's algorithm}. This is [nan] on
        {{!Evidence.Var.Type.is_numeric}non-numeric}
        types or if there are only [nan]s or less than two numbers. *)

    val deviation : ('o, 'a) Var.t -> 'o t -> float
    (** [deviation var d] is [sqrt (deviation var d)], the standard deviation
        of variable [var] in [d]. *)

    (** {1:grouping Grouping} *)

    val group : by:('o, 'a) Var.t -> 'o t -> ('a * 'o t) t
    (** [group ~by:var d] groups observations of [d] by the equivalence
        relation determined by variable [by]. The sequence of groups is ordered
        by [Var.compare_value by]. *)

    (* XXX can't write this :-(, need higher ?
    val group_as_map
        (module Map.S with type 'o t = 'map and type elt = 'a) ->
        ('o, 'a) Var.t -> 'o t -> 'map
    *)

    (** {1:range Range} *)

    val min : ('o, 'a) Var.t -> 'o t -> 'a
    (** [min var d] is the minimal value of [var] in [d] as determined
        by {!Evidence.Var.min_value}. *)

    val max : ('o, 'a) Var.t -> 'o t -> 'a
    (** [min var d] is the maximal value of [var] in [d] as determined
        by {!Evidence.Var.max_value}. *)

    val min_max : ('o, 'a) Var.t -> 'o t -> 'a * 'a
    (** [min_max var d] is [(min var d, max var d)] but more efficient. *)

    val values : ('o, 'a) Var.t -> 'o t -> 'a t
    (** [value var d] is the unique values found in [var] sorted by
        increasing {!Var.compare_value} order. *)

    val dom :
      (module Set.S with type t = 'set and type elt = 'a) ->
      ('o, 'a) Var.t -> 'o t -> 'set
    (** [dom] is {!values} but as a set. *)

    (** {1:transforming Transforming} *)

    val update : ('o, 'a) Var.t -> (int -> 'o -> 'a) -> 'o t -> 'o t
    (** [update var f d] updates variable [var] of each observation of
        [d] with [f]. Note that this fails on observations with absurd
        products. *)

    val set : ('o, 'a) Var.t -> 'a -> int -> 'o t -> 'o t
    (** [set var v i d] sets variable [var] of the [i]th observation
        of [d] to [v]. For efficiency do not call that function repeateadly,
        {!update} is a better option. *)

    val cumsum : ('o, 'a) Var.t -> 'o t -> float t
    (** [cumsum var d] is the
        {{:https://mathworld.wolfram.com/CumulativeSum.html}cumulative sum}
        of variable [var]. *)
  end

  (** {1:conv Converting} *)

  val of_array : ?lend:bool -> ?obs:'o Obs.t -> 'o array -> 'o t
  (** [of_array ?lend ?obs a] is a dataset from [obs] observations in [a].
      Raises [Invalid_argument] if [a] is empty.

      If [lend] is [true] (default) the client lends the array to the
      dataset for its own use, in particular [a] should never be
      modified. *)

  val to_array : ?borrow:bool -> 'o t -> 'o array
  (** [to_array ?borrow d] is [d] as an (non empty) array. If [borrow] is
      [true] the client should never mutate the resulting array (defaults
      to [false]). *)

  val of_list : ?obs:'o Obs.t -> 'o list -> 'o t
  (** [of_list ?obs l] is a dataset from [obs] observations in [l]. Raises
      [Invalid_argument] if [l] is empty. *)

  val to_list : 'o t -> 'o list
  (** [to_list d] is [d] as a (non-empty) list. *)

  val of_t1 : 'a vtype -> 'a list -> ('a, 'a) var * 'a t
  val of_t2 : 'a vtype -> 'b vtype -> ('a * 'b) list ->
    (('a * 'b, 'a) var * (('a * 'b), 'b) var) * ('a * 'b) t

  (** Comma separated values. *)
  module Csv : sig

    type fpath = string
    (** The type for filepaths. *)

    type var_names = string list option
    (** The type for variable names. *)

    (** {1:dec Decoding} *)

    val decode :
      ?var_names:bool -> ?quote:Char.t -> ?sep:Char.t -> ?file:fpath ->
      'o Obs.t -> string -> (var_names * 'o t option, string) result

    val read :
      ?var_names:bool -> ?quote:Char.t -> ?sep:Char.t -> 'o Obs.t -> fpath ->
      (var_names * 'o t option, string) result

    val get : (var_names * 'o t option, string) result -> 'o t

    (** {1:enc Encoding} *)

    val encode :
      ?var_names:bool -> ?quote:Char.t -> ?sep:Char.t -> 'o t -> string
    (** [encode ~var_names ~quote ~sep d] encodes the observations of [d]
        as comma separated values. If [var_names] is [true] (default),
        the variable names are written on the first line. [quote] is the
        character used for quoting values (defaults to ['\"']). [sep]
        is the character used for separating values (defaults to [',']). *)

    val output :
      ?var_names:bool -> ?quote:Char.t -> ?sep:Char.t -> Out_channel.t ->
      'o t -> unit
    (** [output oc d] is like {!encode} but writes the result on [oc]. *)

    val write :
      ?var_names:bool -> ?quote:Char.t -> ?sep:Char.t -> fpath -> 'o t ->
      (unit, string) result
    (** [write] is like {!output} but writes the result in [file]. *)

    (** {1:schema Schema} *)

    type 'a schema = 'a varv list
    val schema_of_string : ?force:'a schema -> string -> bool * unit schema
    val schema_of_file : ?force:'a schema -> string -> bool * unit varv list
    val schema_to_ocaml : 'a schema -> string
  end

  (** {1:fmt Formatting} *)

  val pp :
    ?first:int -> ?last:int -> ?count:count -> ?vars:'o varv list -> unit ->
    'o t fmt
  (** [pp ~header ~vars ()] is a dataset formatter. If [vars] is specified
      only these variables are printed (defaults to {!vars}). [first], [last],
      [count] have the semantics of {!sub}, except empty ranges are allowed. *)

  val pp_top : 'o t fmt
  (** [pp_top] is a formatter for the toplevel. This is [pp ~count:10 ()]. *)

  val show :
    ?ppf:Format.formatter -> ?first:int -> ?last:int -> ?count:count ->
    ?vars:'o varv list -> 'o t -> unit
  (** [show] is like {!pp} but outputs on [ppf] (defaults to
      {!Format.std_formatter}). *)

  (** {1:top Toplevel support} *)

  (** Toplevel support. *)
  module Top : sig
    val show :
      ?ppf:Format.formatter -> ?first:int -> ?last:int -> ?count:int ->
      ?vars:'o varv list -> 'o t -> unit
    (** [show] is {!show}. *)

    val page : 'o t -> unit
    (** [page] is like [show] but does so in your pager. *)
  end
end

(** Projective syntax. *)
module O : sig

  val v : ('o, 'a) Var.t -> ('o -> 'a)
  (** [v var] is {!Var.proj}[ var]. *)

  val c : 'a -> 'o -> 'a
  (** [c] is {!Fun.const} *)

  val is_nan : ('o -> float) -> ('o -> bool)

  val between : ('o -> 'a) -> ('o -> 'a) -> ('o, 'a) Var.t -> ('o -> bool)

  val ( = ) : ('o -> 'a) -> ('o -> 'a) -> ('o -> bool)
  val ( <> ) : ('o -> 'a) -> ('o -> 'a) -> ('o -> bool)
  val ( < ) : ('o -> 'a) -> ('o -> 'a) -> ('o -> bool)
  val ( > ) : ('o -> 'a) -> ('o -> 'a) -> ('o -> bool)
  val ( <= ) : ('o -> 'a) -> ('o -> 'a) -> ('o -> bool)
  val ( >= ) : ('o -> 'a) -> ('o -> 'a) -> ('o -> bool)
  val ( || ) : ('o -> bool) -> ('o -> bool) -> ('o -> bool)
  val ( && ) : ('o -> bool) -> ('o -> bool) -> ('o -> bool)

  val ( + ) : ('o -> int) -> ('o -> int) -> ('o -> int)
  val ( - ) : ('o -> int) -> ('o -> int) -> ('o -> int)
  val ( * ) : ('o -> int) -> ('o -> int) -> ('o -> int)
  val ( / ) : ('o -> int) -> ('o -> int) -> ('o -> int)

  val ( +. ) : ('o -> float) -> ('o -> float) -> ('o -> float)
  val ( -. ) : ('o -> float) -> ('o -> float) -> ('o -> float)
  val ( *. ) : ('o -> float) -> ('o -> float) -> ('o -> float)
  val ( /. ) : ('o -> float) -> ('o -> float) -> ('o -> float)
end

(** {1:todo TODO}

- Maybe we should rather first have a lightweight DSL
  of typed projections. And a {!Var} is just a named typed
  projection. With respect to this the {!O} module DSL may not be such
  a good idea since we switch to polycompare.
- [na]. Add a per variable specific [na] value that is not considered ?
  Seems footgunish, this will only be considered by DSL functions.
  However it's nice to have a notion of [na] to suppress outliers
  without dropping the whole observation. [Option] looks painful.
  On ints. We could use {!min_int}. Mabye not a good idea…
  We should also define a consistent strategy for the treatment of nan
  values, is it really a good idea to silently ignore them ?
- Tools or [nan] handling/discovering.
- {!Var.Ordinal}, rather have ['a -> int] and consider them as
    [is_numeric] ? Not convenient from an OCaml perspective.
- {!Var.Type} provide a case to allow to inject arbitrary arithmetic types
   via first class modules ?
- Summary statistics, rather return [Invalid_argument] on
  non numeric types ?
- Structural changes remain painful as they invalidate
  projectors. How much is it needed during analysis ? Is an untyped
  scheme (TBD) for tidying enough ?
- {!Var.Type.value_set} and {!Var.Type.value_map} without the polymorphic
  constraint on [t] these are only useful locally. Maybe remove.
- What about adding columns via a side hetergenous keymap in {!Dataset.t}.
- {!Dataset} make optional [obs] arguments required ?
- {!Dataset.Var.count} return the typed variables with the result ?
- What about simplifying to [Float], [Nominal], [Any] ?
- Not sure about the whole thing, maybe go back to first reason on
  `Dataset.Var.* functions on a simple sequence.
- {!Dataset} except for [obs], does it make sense to exist if we
  have an immutable sequence, e.g. [Pvec]. There is the non empty invariant,
  what happens if we drop it ?
- Are we not just seeking a DSL for projections and order definition ?
- Negative thoughts. Negative indices are bad, magnitudes are not ? That
  Remains to be shown. Maybe negativity is hard to comprehend once it
  becomes symbolic (vs. literal constants).
- [show] do terminal plots ?
- Dataset.Var.group ~by:Var.t -> with values (facet), single dim. Can we
  generalize ? Variable for a subset of projections out of a sequence
  of var via Obs.o2, Obs.o3…
- {!Obs.set}, raise [Invalid_argument] if [var] is not part of the product.
- Dataset.Csv.decode, fix limitations. Allow obs rebind to variables
  of csv, implement schema sniffing.
- Think on how to best support time and date time. Likely via
    {!Ordinal} but maybe built-in a few goodies.
*)


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
