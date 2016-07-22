type dvar = Types.type_expr
type dim = Types.unit_desc
exception Unknown_base_dimension
val one : Types.unit_desc
val mul : Types.unit_desc -> Types.unit_desc -> Types.unit_desc
val pow : int -> Types.unit_desc -> Types.unit_desc
val inv : Types.unit_desc -> Types.unit_desc
type dsubst = (dvar * dim) list
val link_unit : Types.type_expr -> Types.unit_desc -> unit
val norm : Types.unit_desc -> Types.unit_desc
val unify : Types.unit_desc -> Types.unit_desc -> bool
val dim_moregen :
  bool -> 'a -> (Types.unit_desc * Types.unit_desc) list -> unit
