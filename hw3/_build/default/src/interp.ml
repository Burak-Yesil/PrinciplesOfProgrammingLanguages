open Ast
open Ds

(* 
Kyle Halton
Burak Yasil

*)

let rec apply_proc : exp_val -> exp_val -> exp_val ea_result =
  fun f a ->
  match f with
  | ProcVal (id,body,env) ->
    return env >>+
    extend_env id a >>+
    eval_expr body
  | _ -> error "apply_proc: Not a procVal"
and
 eval_expr : expr -> exp_val ea_result = fun e ->
  match e with
  | Int(n) ->
    return @@ NumVal n
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1+n2)
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1-n2)
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1*n2)
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return @@ NumVal (n1/n2)
  | Let(id,def,body) ->
    eval_expr def >>=
    extend_env id >>+
    eval_expr body
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return @@ BoolVal (n = 0)
  | Proc(id,e)  ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  ->
    eval_expr e1 >>= fun v1 ->
    eval_expr e2 >>= fun v2 ->
    apply_proc v1 v2
  | Abs(e1)      ->
    eval_expr e1  >>=
    int_of_numVal >>= fun n ->
    return @@ NumVal (abs n)
  | Cons(e1, e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    list_of_listVal ev2 >>= fun l ->
    return @@ ListVal(ev1::l)
  | Hd(e1) ->
    eval_expr e1 >>= fun ev1 ->
    list_of_listVal ev1 >>= fun l ->
    if l = [] then error "Empty List"
    else return @@ (List.hd l)
  | Tl(e1) ->
    eval_expr e1 >>= fun ev1 ->
    list_of_listVal ev1 >>= fun l ->
    if l = [] then error "Empty List"
    else return @@ ListVal(List.tl l)
  
  | Record(fs) ->
    if (record_find(fs)) 
    then error "Record: duplicate fields"
    else sequence(List.map (eval_expr) (snd(List.split fs)) ) >>= fun l ->
    return @@ (RecordVal (List.combine (fst (List.split fs)) l  ))
  

  | Proj(e,id) ->
    eval_expr e >>= record_of_recordVal >>= fun rv ->
    record_proj rv id >>= fun v -> return @@ v


  | Empty(e1)  ->
    eval_expr e1 >>= tree_of_treeVal >>= fun t ->
      (match t with
      | Empty -> return @@ (BoolVal true)
      | Node(x,y,z) -> return @@ (BoolVal false))

  | EmptyList -> return @@ ListVal []

  | EmptyTree ->  return @@ TreeVal Empty (* Returns an Empty which is of type tree which is of type exp_val tree which is a Treeval *)
  | Node(e1,lte,rte) -> 
    eval_expr e1 >>= fun ev1 -> (* Want to check if e1 is an expr_val *)
    eval_expr lte >>= (* Want to check if lte is a TreeVal either empty or a node *)
    tree_of_treeVal >>= fun n1 ->
    eval_expr rte >>= (* Want to check if rte is a TreeVal either empty or a node *)
    tree_of_treeVal >>= fun n2 ->
    return @@ TreeVal(Node(ev1, n1, n2)) (* Want to return a Node which is a treeval *)
  | CaseT(target,emptycase,id1,id2,id3,nodecase) ->
    eval_expr target >>= tree_of_treeVal >>= fun t ->
    (match t with
    | Empty ->
      eval_expr emptycase >>= fun emptyV -> return @@ emptyV
    | Node(x,y,z) ->
      extend_env id1 x >>+ extend_env id2 (TreeVal y) >>+ extend_env id3 (TreeVal z) >>+
      eval_expr nodecase >>= fun nodeV -> return @@ nodeV)
      
   
    
  | Tuple(es) ->  failwith "implement me"
  | Untuple(ids,e1,e2) ->  failwith "implement me"
  | _ -> failwith "not implemented"


(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

(* Parse a string into an ast *)


let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let lexer s =
  let lexbuf = Lexing.from_string s
  in Lexer.read lexbuf


(* Interpret an expression *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_expr
  in run c

