(* classic rational implementation over ints *)
(* in normal form whe have den > 0 and gcd(num, den) = 1  *)

module Ratio = struct
exception Null_denominator

type ratio = { num : int ;
               den : int }


let zero = { num = 0 ; den = 1 };;
let of_int n = {num = n ; den = 1};;

(* compute gcd of _positive_ integers *)
let rec gcd p q =
  if p < q then gcd q p
  else
    if q = 0
    then p
    else gcd q (p mod q)
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
  make (p.num * q.den + q.num * p.den) (p.den * q.den)
;;

let minus p q = plus p {num = -q.num ; den = q.den};;


let mult p q =
  make (p.num * q.num) (p.den * q.den)
;;

let div p q =
  mult p (make q.den q.num)
;;

let inv r =
  if r.num = 0 then raise Null_denominator
  else
    let s = if r.num > 0 then 1 else -1 in
    {num = s * r.den ; den = s * r.num}
;;

let less p q = (p.num * q.den < q.num * p.den);;

let greater p q = less q p;;

let abs_val r = {r with num = abs r.num};;
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
  (* call repr on each variable in ud_vars *)
  let vars = List.map (fun (v,e) -> repr v, e) e.ud_vars in
  (* split variables and other types *)
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
        | [] -> m
        | h::t -> let n = if abs (snd h) < abs (snd m) then h else m in
          find_min n t in
      let (v,n) = find_min (List.hd e.ud_vars) e.ud_vars in

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
          let nv = newty2 v.level (Tvar None) in
          (* compute the new substitution *)
          if new_e.ud_vars = [] then false
          else (
            link_unit v (mul { ud_vars = [nv,1] ; ud_base = [] } new_e);
            aux e )
    end in
  aux (mul e1 (inv e2))
;;

(* gaussian elimination *)

(* swap rows i and j in m *)
let swap m i j =
  let r = m.(i) in
  Array.set m i m.(j);
  Array.set m j r
;;

(* multiply row i by x in m *)
let mult_row m x i =
  Array.iteri (fun j y -> Array.set m.(i) j (Ratio.mult x y)) m.(i)
;;

(* perform a := a + x * b in m *)
let add_scal_row x a b =
  let r = Array.map (Ratio.mult x) b in
  let add j y = a.(j) <- Ratio.plus r.(j) y in
  Array.iteri add a
;;


let gauss m =
  let num_rows = Array.length m in
  let num_cols = Array.length m.(0) in
  let r = ref (-1) in
(* find the next pivot, defined as the element on column j which has     *)
(* the largest absolute value and an index greater than r (last pivot) *)
  let abs_less p q = Ratio.less (Ratio.abs_val p) (Ratio.abs_val q) in
  let find_pivot j =
    let max_index = ref (!r+1) in
    for i = !r+1 to num_rows -1 do
      if abs_less m.(!max_index).(j) m.(i).(j)
      then max_index := i
    done ;
    !max_index
  in

  for j = 0 to min num_cols num_rows - 1 do
    let k = find_pivot j in
    if m.(k).(j) <> Ratio.zero then begin
      incr r;
      mult_row m (Ratio.inv m.(k).(j)) k;
      swap m !r k;
      for i = !r + 1 to num_rows -1 do
        add_scal_row (Ratio.minus Ratio.zero m.(i).(j)) m.(i) m.(!r);
      done
    end
  done
;;

module StringSet = Set.Make(String);;

let build_matrix eqlist =
  (* list all variables *)
  let rec sort_units left right base = function
    | [] -> left,right,base
    | (l,r)::q ->
        let lvars = List.map fst l.ud_vars in
        let rvars = List.map fst r.ud_vars in
        let b = List.map fst (List.rev_append l.ud_base r.ud_base) in
        sort_units (lvars::left) (rvars::right) (b::base) q in
  let l,r,b = sort_units [] [] [] eqlist in
  (* sort and eliminate duplicates *)
  let left = TypeSet.elements
      (List.fold_left
         (fun s e -> TypeSet.union s (TypeSet.of_list e))
         TypeSet.empty l)
  and right = TypeSet.elements
      (List.fold_left
         (fun s e -> TypeSet.union s (TypeSet.of_list e))
         TypeSet.empty r)
  and base = StringSet.elements
      (List.fold_left
         (fun s e -> StringSet.union s (StringSet.of_list e))
         StringSet.empty b) in

(* compute matrix dimensions *)
  let num_left = List.length left and
      num_right = List.length right and
      num_base = List.length base in
  let num_rows = List.length eqlist in
  let m = Array.make_matrix num_rows
      (num_left + num_right + num_base) Ratio.zero in

  (* get the index of an element in a list *)
  let index_of x l =
    let rec count n l = match l with
    | h::t -> if h = x then n else count (n+1) t
    | [] -> raise Not_found in
    count 0 l in

  (* write the equation ud1 = ud2 in the i-nth line *)
  let write_eq i ud1 ud2 =
    List.iter (fun (v,e) -> m.(i).(index_of v left) <-
      Ratio.of_int e) ud1.ud_vars;
    List.iter (fun (v,e) -> m.(i).(num_left + index_of v right) <-
      Ratio.of_int (-e)) ud2.ud_vars;
    (* group base variables *)
    let b = (mul ud1 (inv ud2)).ud_base in
    List.iter (fun (v,e) -> m.(i).(num_left + num_right + index_of v base) <-
      Ratio.of_int e) b in

  (* fill in the matrix *)
  let rec fill_mat i = function
    | [] -> ()
    | (ud1,ud2)::t ->
        write_eq i ud1 ud2 ;
        fill_mat (i+1) t in
  fill_mat 0 eqlist;
  m
;;

let dim_moregen inst_nongen link eqlist =
  let m = build_matrix eqlist in
  (* TODO *)
  if inst_nongen then
    gauss m
  else
    gauss m
;;
