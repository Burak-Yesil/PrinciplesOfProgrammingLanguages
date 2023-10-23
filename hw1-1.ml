(* 
   Burak Yesil
   I pledge my honor system that I have abided by the stevens system. 
*)




let letter_e = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]
type program = int list
let letter_e = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]

let rec map : ('a ->'b ) -> 'a list -> 'b list =
  fun f l ->
  match l with
  | [] -> []
  | h::t -> f h :: map f t


(* Function 1: Mirror Image *)
let reflect_it x = 
    if x=2 then 4 else (if x=4 then 2 else (if x=3 then 5 else (if x=5 then 3 else x)))

let mirror_image l = map reflect_it l


(* Function 2: rotate_90_letter *)
let rotate_it x = 
    if x=2 then 3 else (if x=4 then 5 else (if x=3 then 4 else (if x=5 then 2 else x)))

let rotate_90_letter l = map rotate_it l


(* Function 3: Rotate_90_word *)
let rotate_90_word l = map rotate_90_letter l


(* Function 4: Repeat *)
let rec repeat n x = 
    match n with
    | 0->[]
    | m->x :: repeat (n-1) x

(* Function 5: Pantograph *)
let rec repeat' n x = 
    match x with
    | 0 -> [0]
    | 1 -> [1]
    | _ -> repeat n x 

let pantograph n l = 
    List.flatten (List.map (repeat' n) l)

let rec pantograph_nm n p = 
    match p with 
    | [] -> []
    | h::t -> if h=0 || h=1 then h::pantograph_nm n t 
    else repeat n h @ pantograph_nm n t

let pantograph_f n l = List.fold_left(fun x y -> x @ (repeat' n y)) [] l


(* Function 6 Coverage *)
let point_conversion (x,y) a  = 
    match a with
    | 2 -> (x,y+1)
    | 4 -> (x,y-1)
    | 3 -> (x+1,y)
    | 5 -> (x-1, y) 
    | 0 | 1 -> (x,y) 
    | _ -> failwith "Invalid Input"


let rec coverage_helper (x,y) l = 
    match l with 
    | [] -> []
    | h::t -> point_conversion (x,y) h :: (coverage_helper (point_conversion (x,y) h) t)

let coverage (x,y) l = 
    (x,y) :: coverage_helper (x,y) l



(* Function 7: Compress *)
let rec count n l c = 
    match l with
    | [] -> 1
    | h::t -> if h!=n then c  else count n t c+1

let rec leftoff n l =
    match l with 
    | [] -> []
    | h::t -> if h!=n then l else leftoff n t

let rec compress l = 
    match l with
    | [] -> []
    | h::t -> if h=0 || h=1 then (h,1) :: compress t else 
        (h, count h t 1) :: compress (leftoff h t)


(* Function 8: uncompress *)
let rec uncompress l = 
    match l with
    | [] -> []
    | h::t -> (repeat (snd h) (fst h) @ uncompress t)

let uncompress_m_helper (x,y) = 
    repeat y x

let uncompress_m l =  List.flatten(List.map (uncompress_m_helper) l)

let uncompress_f l = List.fold_left(fun x y -> x @ (repeat (snd y) (fst y))) [] l

(* Function 9: Optimize *)
let rec removeRedundancy l penUp first_element =
    match l with
    | [] -> []
    | h::t -> if h=0 || h=1 then
            if first_element=true then 
                if h=1 then [] :: removeRedundancy t true false else [h] :: removeRedundancy t false false
            else if h=1 then if penUp=true then removeRedundancy t true false else [h] :: removeRedundancy t true false
                else 
                    if penUp=true then [h] :: removeRedundancy t false false else removeRedundancy t false false
        else [h] :: removeRedundancy t false false

let optimize l = List.flatten(removeRedundancy l true true)



