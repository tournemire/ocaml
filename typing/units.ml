open Types;;
open Btype;;

let one = { ud_vars = [] ;
            ud_base = [] }
;;

(* as merge is used, the fields vars and base need to be sorted lists *)
let mul e1 e2 =
  let merge = List.merge (fun a b -> compare (fst a) (fst b)) in
  let sort = List.sort (fun a b -> compare (fst a).id (fst b).id) in
  let rec add l = match l with
  | [] | [_] -> l
  | (x,n)::(y,m)::t when x = y -> (x, n + m)::(add t)
  | x::t -> x::(add t) in
  let filter l = List.filter (fun (_,n) -> n <> 0 ) l in
  (* eliminate variables with exponent zero *)
  { ud_vars = filter (add (sort (e1.ud_vars @ e2.ud_vars))) ;
    ud_base = filter (add (merge e1.ud_base e2.ud_base)) }
;;

let pow n e =
  let f l = List.map (fun (a,b) -> (a, n * b)) l in
  { ud_vars = f e.ud_vars ;
    ud_base = f e.ud_base }
;;

let inv = pow (-1);;

(* test whether n is a common divisor of all exponents in e *)
let common_divisor n e =
  (* need eta-expansion to be polymorph *)
  let test l = List.for_all (fun x -> snd x mod n = 0) l in
  test e.ud_base && test e.ud_vars
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
let unify link_unit e1 e2 =
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



module StringSet = Set.Make(String);;

type column_info =
  | Left of type_expr
  | Right of type_expr
  | Base of string
;;

let build_matrix eqlist =
  let eqlist = List.map (fun (ud1,ud2) -> norm ud1, norm ud2) eqlist in

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
      (num_left + num_right + num_base) 0 in
  (* build the types array *)
  let left_info = Array.map (fun t -> Left t) (Array.of_list left) in
  let right_info = Array.map (fun t -> Right t) (Array.of_list right) in
  let base_info = Array.map (fun s -> Base s) (Array.of_list base) in
  let columns_info =
    Array.append left_info (Array.append right_info base_info) in

  (* get the index of an element in a list *)
  let index_of x l =
    let rec count n l = match l with
    | h::t -> if h = x then n else count (n+1) t
    | [] -> raise Not_found in
    count 0 l in

  (* write the equation ud1 = ud2 in the i-nth line *)
  let write_eq i ud1 ud2 =
    List.iter (fun (v,e) -> m.(i).(index_of v left) <- e) ud1.ud_vars;
    List.iter (fun (v,e) -> m.(i).(num_left + index_of v right) <-
      -e) ud2.ud_vars;
    (* group base variables *)
    let b = (mul ud1 (inv ud2)).ud_base in
    List.iter (fun (v,e) -> m.(i).(num_left + num_right + index_of v base) <-
      e) b in

  (* fill in the matrix *)
  let rec fill_mat i = function
    | [] -> ()
    | (ud1,ud2)::t ->
        write_eq i ud1 ud2 ;
        fill_mat (i+1) t in
  fill_mat 0 eqlist;
  m,columns_info,num_left,num_right,num_base
;;



(* divide r by x in m *)
let div_row r c =
  Array.iteri (fun j y ->  r.(j) <- y / c) r
;;

let smallest_elt m used vars =
  let current = ref 0 in
  let x,y = ref 0, ref 0 in
  let n = Array.length m and
      p = Array.length vars in
  for i = 0 to n-1 do
    (* search only in unused equations *)
    if not used.(i) then
      for j = 0 to p-1 do
        let candidate = m.(i).(j) in
        let is_variable = vars.(j) in

        if is_variable &&
          candidate <> 0 &&
          (!current = 0 || abs candidate <= abs !current)

        then (x := i ; y := j ; current := candidate)
      done
  done ;
  !x,!y,!current
;;

(* supposing that m.(x).(y) = 1 or -1 *)
(* eliminate var y from unused equations (i <> x)*)
let eliminate m used x y =
  let c = m.(x).(y) in
  let n = Array.length m and
      p = Array.length m.(0) in
  for i = 0 to n-1 do
    (* work only on unused equations *)
    if not used.(i) && x <> i then begin
      for j = 0 to p-1 do
        if j <> y then
          m.(i).(j) <- m.(i).(j) - m.(i).(y) * c * m.(x).(j)
      done ;
      m.(i).(y) <- 0
    end
  done ;
;;

let divides_vars r vars y =
  let check = Array.map2 (fun b elt -> not b || elt mod r.(y) = 0) vars r in
  check = Array.make (Array.length r) true
;;

let divides_nonvars r vars y =
  let check = Array.map2 (fun b elt -> b || elt mod r.(y) = 0) vars r in
  check = Array.make (Array.length r) true
;;

let newvar m vars used x y =
  let c = m.(x).(y) in
  let n = Array.length m and
      p = Array.length m.(0) in
  (* rewrite the original equation with the new variable *)
  let r = m.(x) in
  let old_var_subst =
    Array.mapi (fun j e-> if vars.(j) then -e/c else 0) r in
  (* actually not the real expression *)
  (* the substitution matrix is I_n replacing the y-th line by old_var_subst  *)
  old_var_subst.(y) <- 1;

  Array.iteri (fun j e -> if vars.(j) then r.(j) <- e mod c) r;
  r.(y) <- c ;

  for i = 0 to n-1 do
    (* work only on unused equations *)
    if not used.(i) && x <> i then begin
      for j = 0 to p-1 do
        if j <> y && vars.(j) then
          m.(i).(j) <- m.(i).(j) + m.(i).(y) * old_var_subst.(j)
      done
    end
  done ;
  old_var_subst
;;

(* extended Euclidean algorithm (Knuth, TAOCP vol.2) *)
(* vars : array determining instanciable variables  *)
let knuth m vars =
  let n = Array.length m in
  (* register which equations have been used *)
  let used = Array.make n false in

  let rec aux substs =
    let x,y,c = smallest_elt m used vars in
    if c = 0 then true, substs else

    if c = 1 then begin
      (* eliminate var n°y in all equations *)
      eliminate m used x y;
      used.(x) <- true;    (* mark equation x as used *)
      aux substs
    end else begin
      (* c divides all variables coeffs  *)
      if divides_vars m.(x) vars y
      then begin
        (* c also divides nonvariables coeffs *)
        if divides_nonvars m.(x) vars y
        then begin
          div_row m.(x) c;
          eliminate m used x y;
          aux substs
        end

        else false, substs          (* no solution *)
      end
      else
        aux ((y, newvar m vars used x y)::substs)
    end
  in aux []
;;


(* compute the expression of row when replacing entry x with *)
(* its expression var *)
let subst_one_var row var x =
  let c = row.(x) in
  let f i e = row.(i) <- e + c * var.(i) in
  Array.iteri f row;
  row.(x) <- c * var.(x)
;;

let rec solve = function
  | [] -> []
  | (i,var)::q ->
      (* build the sublist of equations containing var i *)
      let sub = solve q in
      List.iter (fun (_,row) -> subst_one_var row var i) sub;
      if List.exists (fun (j,_) -> j = i) sub then sub else (i,var)::sub
;;

let dim_moregen inst_nongen may_inst link eqlist =
  let m,columns_info,nleft,nright,nbase = build_matrix eqlist in
  if Array.length m = 0 || Array.length m.(0) = 0 then true else
  let nvars = nleft + nright in

  (* unwrap and separate variables and base units *)
  let unwrap cols_info nv nb =
    let typevars = Array.make nv (newgenty Tnil)
    and base = Array.make nb "" in
    let distribute i = function
      | Left tvar | Right tvar -> typevars.(i) <- tvar
      | Base s -> base.(i - nv) <- s in
    Array.iteri distribute cols_info;
    typevars,base in
  let typevars, base = unwrap columns_info nvars nbase in
  let is_var = Array.map may_inst typevars in

  (* apply knuth algorithm to the equation system *)
  let success, subst_list = knuth m is_var in
  if success && inst_nongen then begin
    (* instantiate according to the substitutions given by the algo *)
    let sol = solve subst_list in
    let newtypevars = Array.copy typevars in
    (* generates fresh vars for every substituted var *)
    List.iter (fun (i,_) ->
      newtypevars.(i) <- newty2 typevars.(i).level (Tvar None)) sol;
    let f subst =
      let filter_nonzeros a =
        List.filter (fun (_,x) -> x <> 0) (Array.to_list a) in
      let ud_vars = Array.mapi (fun i t -> t,subst.(i)) newtypevars in
      let ud_vars = filter_nonzeros ud_vars in
      let ud_base = Array.mapi (fun i t -> t,subst.(i + nvars)) base in
      let ud_base = filter_nonzeros ud_base in
      {ud_vars ; ud_base} in
    List.iter (fun (i,s) -> link typevars.(i) (f s)) sol
  end ;
  success
;;

let dim_eqtype subst dim_eqs =
  let m,columns_info,nleft,nright,_ = build_matrix dim_eqs in
  if Array.length m = 0 || Array.length m.(0) = 0 then true else

  let vars_info = Array.sub columns_info 0 (nleft + nright) in
  let is_var =
    Array.map (function Left _ -> true | _ -> false) vars_info in
  (* add equations from subst *)
  let unwrap = function
    | Left t | Right t -> t
    | _ -> assert false in
  let typevars = Array.map unwrap vars_info in

  (* look for dimensional correspondances in subst and generate *)
  (* associated equations to add in the matrix *)
  let rec pair_equations =
    (* return the couple : *)
    (* t exists in a, if t exists then index else length of a  *)
    let index_of t a =
      let n = Array.length a in
      let i = ref 0
      and found = ref false in
      while !i < n && not !found do
        if t = a.(!i) then found := true else incr i
      done ;
      !found, !i in
    (* build the array with 1 in i1, -1 in i2 and 0 elsewhere *)
    let eq_of_var_pair i1 i2 =
      let r = Array.make (nleft + nright) 0 in
      r.(i1) <- 1 ; r.(i2) <- -1;
      r in
    function
      | [] -> []
      | (t1,t2)::q ->
          let exist1,i1 = index_of t1 typevars in
          let exist2,i2 = index_of t2 typevars in
          let eqs = pair_equations q in
          if exist1 && exist2 then (eq_of_var_pair i1 i2)::eqs
          else eqs in
  let m = Array.append m (Array.of_list (pair_equations subst)) in


  let m' = Array.map Array.copy m in

  let left_moregen_right, _ = knuth m is_var in
  let right_moregen_left, _ = knuth m' (Array.map not is_var) in
  left_moregen_right && right_moregen_left
;;
