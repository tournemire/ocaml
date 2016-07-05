module Ratio :
  sig
    exception Null_denominator
    type ratio = { num : int; den : int; }
    val zero : ratio
    val gcd : int -> int -> int
    val lcm : int -> int -> int
    val make : int -> int -> ratio
    val plus : ratio -> ratio -> ratio
    val minus : ratio -> ratio -> ratio
    val mult : ratio -> ratio -> ratio
    val div : ratio -> ratio -> ratio
    val isinf : ratio -> ratio -> bool
    val issup : ratio -> ratio -> bool
  end
type dvar = Types.type_expr
type dim = Types.unit_desc
exception Dim_unification_failed of dim * dim
exception Unknown_base_dimension
val one : Types.unit_desc
val mul : Types.unit_desc -> Types.unit_desc -> Types.unit_desc
val list_mul : Types.unit_desc list -> Types.unit_desc
val pow : int -> Types.unit_desc -> Types.unit_desc
val inv : dim -> Types.unit_desc
type dsubst = (dvar * dim) list
val dsubst : dsubst -> dim -> dim
val dcomp : dsubst -> dsubst -> dsubst
val common_divisor : int -> Types.unit_desc -> bool
val link_unit : Types.type_expr -> Types.unit_desc -> unit
val unify : dim -> dim -> bool
