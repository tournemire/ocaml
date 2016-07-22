module Ratio :
  sig
    exception Null_denominator
    type ratio = { num : int; den : int; }
    val zero : ratio
    val of_int : int -> ratio
    val gcd : int -> int -> int
    val lcm : int -> int -> int
    val make : int -> int -> ratio
    val plus : ratio -> ratio -> ratio
    val minus : ratio -> ratio -> ratio
    val mult : ratio -> ratio -> ratio
    val div : ratio -> ratio -> ratio
    val inv : ratio -> ratio
    val less : ratio -> ratio -> bool
    val greater : ratio -> ratio -> bool
    val abs_val : ratio -> ratio
  end
type dvar = Types.type_expr
type dim = Types.unit_desc
exception Unknown_base_dimension
val one : Types.unit_desc
val mul : Types.unit_desc -> Types.unit_desc -> Types.unit_desc
val list_mul : Types.unit_desc list -> Types.unit_desc
val pow : int -> Types.unit_desc -> Types.unit_desc
val inv : Types.unit_desc -> Types.unit_desc
type dsubst = (dvar * dim) list
val dsubst : dsubst -> dim -> dim
val dcomp : dsubst -> dsubst -> dsubst
val common_divisor : int -> Types.unit_desc -> bool
val link_unit : Types.type_expr -> Types.unit_desc -> unit
val norm : Types.unit_desc -> Types.unit_desc
val unify : Types.unit_desc -> Types.unit_desc -> bool
val swap : 'a array -> int -> int -> unit
val mult_row : Ratio.ratio array array -> Ratio.ratio -> int -> unit
val add_scal_row :
  Ratio.ratio -> Ratio.ratio array -> Ratio.ratio array -> unit
val gauss : Ratio.ratio array array -> unit
module StringSet :
  sig
    type elt = String.t
    type t = Set.Make(String).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val of_list : elt list -> t
  end
val build_matrix :
  (Types.unit_desc * Types.unit_desc) list -> Ratio.ratio array array
