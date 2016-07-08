(* classic rational implementation over ints *)
(* in normal form whe have den > 0 and gcd(num, den) = 1  *)

module Ratio = struct
    exception Null_denominator;;

    type ratio = { num : int ;
                   den : int }
    ;;

    let zero = { num = 0 ; den = 1 };;

    (* compute gcd of _positive_ integers *)
    let rec gcd p q =
      if p < q then gcd q p
      else
        if q = 0
        then p
        else gcd q (p mod q)
    ;;

    let lcm p q =
      let g = gcd p q in
      p / g * q
    ;;

    let make a b =
      if b = 0
      then raise Null_denominator
      else if a = 0 then zero
      else
        let s_a = if a > 0 then 1 else -1 in
        let s_b = if b > 0 then 1 else -1 in
        let g = s_b * (gcd (a * s_a) (b * s_b)) in
        { num = a / g ; den = b / g}
    ;;

    let plus p q =
      let l = lcm p.den q.den in
      let a = l / p.den in
      let b = l / q.den in
      { num = p.num * a + q.num * b ;
        den = l }
    ;;

    let minus p q = plus p {num = -q.num ; den = q.den};;


    let mult p q =
      make (p.num * q.num) (p.den * q.den)
    ;;

    let div p q =
      mult p (make q.den q.num)
    ;;

    let isinf p q = (p.num * q.den <= q.num * p.den);;
    let issup p q = isinf q p;;

  end

open Types;;
open Btype;;

type dvar = type_expr;;
type dim = unit_desc;;

exception Unknown_base_dimension;;

let one = { ud_vars = [] ;
            ud_base = [] }
;;

(* as merge is used, the fields vars and base need to be sorted lists *)
let mul e1 e2 =
  let merge l1 l2 = List.merge (fun a b -> compare (fst a) (fst b)) l1 l2 in
  let rec add l = match l with
    | [] | [_] -> l
    | (x,n)::(y,m)::t when x == y -> (x, n + m)::(add t)
    | x::t -> x::(add t) in
  let filter l = List.filter (fun (_,n) -> n <> 0 ) l in
  (* eliminate variables with exponent zero *)
  { ud_vars = filter (add (merge e1.ud_vars e2.ud_vars)) ;
    ud_base = filter (add (merge e1.ud_base e2.ud_base)) }
;;

let rec list_mul = function
  | [] -> one
  | h::t -> mul h (list_mul t)
;;

let pow n e =
  let f l = List.map (fun (a,b) -> (a, n * b)) l in
  { ud_vars = f e.ud_vars ;
    ud_base = f e.ud_base }
;;

let inv = pow (-1);;

type dsubst = (dvar * dim) list ;;

let dsubst : dsubst -> dim -> dim =
  fun s e ->
  let in_dom x = List.mem_assoc (fst x) s in
  let dom,fix = List.partition in_dom e.ud_vars in
  let l = List.map (fun (v,n) -> pow n (List.assoc v s)) dom in
  mul (list_mul l) {ud_vars = fix ; ud_base = e.ud_base}
;;

(* compose substitutions *)
let dcomp : dsubst -> dsubst -> dsubst =
  fun s t ->
  (* apply s to images of t *)
  let u = List.map (fun (v,im) -> v,dsubst s im) t in
  (* add substitutions of s that are not in dom(t)  *)
  (List.filter (fun (v,_) -> not (List.mem_assoc v t)) s) @ u
;;

(* test whether n is a common divisor of all exponents in e *)
let common_divisor n e =
  (* need eta-expansion to be polymorph *)
  let test l = List.for_all (fun x -> snd x mod n = 0) l in
  test e.ud_base && test e.ud_vars
;;

let link_unit tv ud =
  link_type tv (newty2 tv.level (Tunit ud))
;;

let rec norm e =
  (* repr sur tous les membres gauches de ud_vars *)
  let vars = List.map (fun (v,e) -> repr v, e) e.ud_vars in
  (* partitionner variables et autres *)
  let vars,notvars = List.partition (fun (v,_) -> is_Tvar v) vars in
  List.fold_left (fun ud (t,e)-> match t.desc with
                                   Tunit ud' -> mul ud (pow e (norm ud'))
                                 | _ -> assert false )
                 {e with ud_vars = vars} notvars
;;


(* try to unify e1 and e2, return true if succeded *)
let unify e1 e2 =
  let rec aux e =
    (* substitution, multiplication etc... ensure that variables *)
    (* with exponent zero are eliminated *)
    let e = norm e in
    if e.ud_vars = []
    then List.for_all (fun (_,n) -> n = 0) e.ud_base
    else begin
        (* find the variable with the smallest non-zero exponent  *)
        let rec find_min m = function
          | [] -> assert false  (* e.ud_vars id checked above *)
          | [h] -> if abs (snd h) < abs (snd m) then h else m
          | h::t -> let n = if abs (snd h) < abs (snd m) then h else m in
                    find_min n t in
        let (v,n) = find_min ({desc = Tnil ; level = 0 ; id=0}, max_int) e.ud_vars in

        if n < 0
        then aux (inv e)
        else
          let divide_exponents l =
            List.map (fun (x,y) -> (x, - y/n)) l in
          let new_e = { ud_vars = List.remove_assoc v (divide_exponents e.ud_vars) ;
                        ud_base = divide_exponents e.ud_base } in
          if common_divisor n e then
            ( link_unit v new_e ; true )
          else
            (* generate a new variable *)
            (* prendre une nv variable de même niveau que la précédente *)
            let nv = newty2 v.level (Tvar None)  in
            (* compute the new substitution *)
            if new_e.ud_vars = [] then false
            else (
              link_unit v (mul { ud_vars = [(nv,1)] ; ud_base = [] } new_e);
              aux e )
      end in
  aux (mul e1 (inv e2))
;;
