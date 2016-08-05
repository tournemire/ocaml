val one : Types.unit_desc
val mul : Types.unit_desc -> Types.unit_desc -> Types.unit_desc
val pow : int -> Types.unit_desc -> Types.unit_desc
val inv : Types.unit_desc -> Types.unit_desc
val norm : Types.unit_desc -> Types.unit_desc
val unify : Types.unit_desc -> Types.unit_desc -> bool
val dim_moregen :
  bool ->
  (Types.type_expr -> bool) ->
  (Types.type_expr -> Types.type_expr -> unit) ->
  (Types.unit_desc * Types.unit_desc) list -> bool
val dim_eqtype :
  (Types.type_expr * Types.type_expr) list ->
  (Types.unit_desc * Types.unit_desc) list -> bool
