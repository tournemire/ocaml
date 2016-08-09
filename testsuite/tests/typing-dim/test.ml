module Dim : sig
    type 'a dfloat = private float
    val create : float ->  dfloat [`a]
    val (+:) : dfloat [`a] -> dfloat [`a] -> dfloat [`a]
    val ( *: ) : dfloat [`a] ->  dfloat [`b] -> dfloat [`a * `b]
    val ( /: ) : dfloat [`a] ->  dfloat [`b] -> dfloat [`a / `b]
    val inv :  dfloat [`a] -> dfloat [1 / `a]
  end = struct

    type 'a dfloat = float ;;

    let create f = f
    let ( +: ) = ( +. )
    let ( *: ) = ( *. )
    let ( /: ) = ( /. )
    let inv f = 1. /. f
  end
;;

open Dim;;
let x : dfloat [m] = create 3.;;
