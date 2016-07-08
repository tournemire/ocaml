module Ratio :
  sig
    exception Null_denominator
    type ratio = { num : int; den : int; }
    val zero : ratio
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
exception Unknown_base_dimension
val one : dim
val mul : dim -> dim -> dim
val list_mul : dim list -> dim
val pow : int -> dim -> dim
val inv : dim -> dim
type dsubst = (dvar * dim) list
val dsubst : dsubst -> dim -> dim
val dcomp : dsubst -> dsubst -> dsubst
val link_unit : Types.type_expr -> dim -> unit
val norm : dim -> dim
val unify : dim -> dim -> bool
