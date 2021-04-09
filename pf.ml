#load "expression_scanner.cmo";;
open Expression_scanner;;

(*Type operator et tree*)
type operator = |Plus | Minus | Mult | Div | Null
type tree = 
  | Var of char
  | Cst of int
  | Unary of tree
  | Binary of operator * tree * tree
;;


(*Indique l'aridité d'un token
  @Param t => token
*)
let aridity t =
  match t with
  | Add -> 2
  | Subtract -> 2
  | Multiply -> 2
  | Divide -> 2
  | Minus -> 2
  | Variable t-> 0
  | Number t-> 0
  | End -> 1
;;




(*Gestion des operateurs
  @Param token => token
  @Param tree_list => tree list
*)
let to_tree (token, tree_list : token * tree list) =
  let hd::tl = tree_list in 
  let hd2::tl2 = tl in 
  match token with
  | Add       -> Binary(Plus, hd2, hd)::tl2
  | Subtract  -> Binary(Minus, hd2, hd)::tl2
  | Multiply  -> Binary(Mult, hd2, hd)::tl2
  | Divide    -> Binary(Div, hd2, hd)::tl2
  | Minus     -> Unary(hd)::tl
  | _ -> failwith("to_tree : token non valide")
  
;;


(*Fonction auxiliare de parse
  @Param exp => token
  @Param t_list => tree list
*)
let rec parse_bis (exp,t_list) =
  if exp = []
  then failwith("parse_bis : fin d'expression attendue")
  else
    let hd::tl = exp in
    match hd with
    | Variable(x) -> parse_bis( tl, Var(x)::t_list)
    | Number(x)   -> parse_bis( tl, Cst(x)::t_list)
    | End         -> List.hd t_list
    | _           -> parse_bis( tl, to_tree(hd, t_list))
  ;;

(*Convertie l'expression tokene en arbre
  @Patam exp => token
*)
let parse exp =
  parse_bis(exp,[])
  ;;



  (* Fonction auxiliaire de la simplification d'un arbre 
    @Param t_tree => tree
  *)
  let simplify_bis t_tree =
    match t_tree with
    | Cst(x)  -> t_tree
    | Var(x)  -> t_tree
    | Unary(x)-> t_tree
    | Binary(op,lson,rson) -> match op with
                              |Plus   -> ( match lson with
                                          |Cst(x) -> (if(x = 0)
                                                      then rson
                                                      else
                                                      match rson with
                                                      |Cst(y) ->  Cst(x+y)
                                                      |_ -> t_tree )
                                          |Var(x) -> (match rson with
                                                      |Cst(y) ->  if(y = 0)
                                                                  then lson
                                                                  else t_tree 
                                                      |_ -> t_tree )
                                          |_ -> t_tree)
                              |Minus  -> ( match lson with
                                          |Cst(x) -> (match rson with
                                                      |Cst(y) ->  Cst(x-y)
                                                      |Var(y) ->  t_tree
                                                      |_ -> t_tree )
                                          |Var(x) -> (match rson with
                                                      |Cst(y) ->  t_tree
                                                      |Var(y) ->  if (x=y)
                                                                  then Cst(0)
                                                                  else t_tree
                                                      |_ -> t_tree )
                                          |_ -> t_tree )
                              |Div    -> ( match lson with
                                          |Cst(x) -> (match rson with
                                                      |Cst(y) ->  Cst(x/y)
                                                      |Var(y) ->  t_tree
                                                      |_ -> t_tree )
                                          |Var(x) -> (match rson with
                                                      |Cst(y) ->  t_tree
                                                      |Var(y) ->  if (x=y)
                                                                  then Cst(1)
                                                                  else t_tree
                                                      |_ -> t_tree )
                                          |_ -> t_tree )
                              |Mult   -> ( match lson with
                                          |Cst(x) ->  (match rson with
                                                       |Cst(y) ->  Cst(x*y)
                                                       |Var(y) ->  t_tree
                                                       |_ -> t_tree )
                                          |Var(x) -> (match rson with
                                                      |Cst(y) ->  if(y = 0)
                                                                  then Cst(0)
                                                                  else
                                                                    if(y = 1)
                                                                    then lson
                                                                    else t_tree
                                                      |Var(y) -> t_tree
                                                      |_ -> t_tree )
                                          |_ -> t_tree)                          
                              |_ -> failwith("simplify_bis : operateur non reconnu") 
;;

  
(*Simplifie un arbre
  @Param tree => tree
*)
let rec simplify tree =
  match tree with
  |Binary(op, lson, rson) -> simplify_bis(Binary(op,simplify(lson),simplify(rson)))
  |_                      -> tree
  ;;



(*Converti un opérateur en string
  @Param v => operator
*)
let change_op_in_expression v =
  match v with
  | Plus ->  "+";
  | Minus ->"-";
  | Mult -> "*";
  | Div -> "/";
;;


(* converti un charactère en string*)
let string_of_char = String.make 1 ;;


let rec abr_to_string_aux(tree,op_prec)=
  match tree with
  | Binary(op, lson, rson) -> if(op_prec = op)
                              then abr_to_string_aux(lson,op) ^ change_op_in_expression(op) ^ abr_to_string_aux(rson,op)
                              else "(" ^ abr_to_string_aux(lson,op) ^ change_op_in_expression(op) ^ abr_to_string_aux(rson,op) ^ ")"
  | Unary(x)               -> "(-" ^ abr_to_string_aux(x,Null) ^ ")"
  | Cst(x)                 -> string_of_int(x)
  | Var(x)                 -> string_of_char(x)
;;



(* converti un arbre en string
  @Param tree => tree
*) 
let abr_to_string tree =
  match tree with
  | Binary(op, lson, rson) -> abr_to_string_aux(lson,op) ^ change_op_in_expression(op) ^ abr_to_string_aux(rson,op)
  | Unary(x)               -> abr_to_string_aux(x,Null)
  | Cst(x)                 -> string_of_int(x)
  | Var(x)                 -> string_of_char(x)
;;
    

(*Fonction pour les tests
  Converti token list en string
  @Param t_list => token list
*)
let tokenList_to_expression t_list =
  let tree = parse t_list in
  let simp_tree = simplify tree in
  let expr = abr_to_string simp_tree in
  expr
  ;;


(*Test*)
tokenList_to_expression [Variable('x'); Number(3); Add; Number(5); Number(7); Add; Add; Number(3); Number(4); Multiply; Number(1); Number(3); Add; Divide;Divide; End];;


(*
(*Tentative d'un main (cf. compilation) *)
let main =
  print_endline("Entrer une expression : ");
  let stg_exp = input_to_token_list() in

  let tree = parse(stg_exp) in
  let tree_simplify = simplify tree in
  let stg_tree = abr_to_string tree in
  let stg_tree_simplify = abr_to_string tree_simplify in

  print_endline("Affiche expression non simplifiée : ");
  print_endline(stg_tree);
  print_endline("Affiche expression simplifiée : ");
  print_endline(stg_tree_simplify)
;;
*)
