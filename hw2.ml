(*Burak Yesil*)
(*I pledge my honor that I have abided by the stevens honor system.*)

(* Defining trees *)
type 'a gt = Node of 'a*('a gt) list

let t : int gt =
 Node (33, [Node (12,[]); 
            Node (77, 
              [Node (37, 
                     [Node (14, [])]); 
               Node (48, []); 
               Node (103, [])])
       ])

(* How to create a node *)
let mk_leaf (n:'a) : 'a gt =
  Node(n,[])

(* Function 1: returns the height *)
let rec height t = 
    match t with
    | Node (_, []) -> 1
    | Node (_, e) -> 1 + List.fold_left(fun x y -> if x > y then x else y) 0 (List.map height e)

(* Function 2: returns the number of nodes *)
let rec size t = 
    match t with
    | Node (_, []) -> 1
    | Node (_, e) -> 1 + List.fold_left (fun a b -> a + b) 0 (List.map size e)

(* Function 3: returns list of all paths *)
let rec paths_to_leaves t =
    match t with
    | Node(_, []) -> [[]]
    | Node(_, e) -> List.concat(
         List.mapi (fun i l -> (
             List.map (fun x -> i::x) l)) 
             (List.map paths_to_leaves e))  

(* Function 4: returns whether or not all leaves are same level *)
let rec is_leaf_perfect t =
  match t with
  | Node (_, []) -> true
  | Node (_, h::t) -> if((List.fold_left (fun x y -> if (x=y) then 1 else 0)
     (height h) (List.map height t))=1) then true else false

(* Function 5: returns the preorder traversal *)
let rec preorder t =
  match t with
  | Node (x, []) -> [x]
  | Node (x, y) -> x::List.flatten(List.map preorder y)

(* Function 6: returns the mirror image of tree *)
let rec mirror t = 
  match t with 
  | Node (h, []) -> Node(h, [])
  | Node (h, l) -> Node (h, List.rev (List.map mirror l))


(* Function 7: maps function f to nodes on tree t *)
let rec mapt f (t) = match t with | Node(h, t) -> Node(f h, List.map (mapt f) t)


(* Function 8: encodes the recursion scheme over general trees *)
let rec foldt f (t) = match t with | Node(h, t) -> f h (List.map(foldt f) t)

let sumt t = foldt (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t

let memt t e = foldt (fun i rs -> i=e || List.exists (fun i -> i) rs) t


(* Function 9 *)
let mirror' t  = foldt (fun d l -> Node(d, List.rev l)) t