(** CW1, QUESTION 1 **)

(* a (3 marks) *)
(*let all p xs = failwith "Not implemented"*)

let rec all p xs =
   match xs with
   |[] -> true  (* an empty list satisfies the condition. *)
   |x :: r -> if p x 
      then all p r 
      else false;;
 
let is_even n = n mod 2 = 0;;

(* b (3 marks) *)


let rec exists p xs =
   match xs with
   |[] -> false
   |x :: r -> if p x then true else exists p r;;


(* c (2 marks) *)
(* Provide your answer in this comment (you are free to implement) allF if you find it useful!

two functions have almost same outputs. in the all function implemented using explicit recursion, when the list is empty, it returns true. 
in the allF function when the list is empty it returns false, because it starts with an initial accumulator value of false
so basicaly here it will return true ⬇️
let r1 = all is_even [];;  
and here func will rerurn false ⬇️
let r2 = allF is_even [];; 

This the only difference between all func and allF

*)

(* d (2 marks) *)
(* Once you figure out what parameters allSpec and existsSpec should
   take, replace the dummy paramter () with the chosen ones *)

let rec member x xs =
   match xs with
   |[] -> false
   |y :: ys -> (x = y) || member x ys;;
   
let rec allSpec p xs =
   match xs with
   |[] -> true
   |y :: ys -> p y && allSpec p ys;;
   
let rec existsSpec p xs =
   match xs with
   |[] -> false
   |y :: ys -> p y || existsSpec p ys;;

(*
allSpec: ('a -> bool) -> 'a list -> bool
existsSpec: ('a -> bool) -> 'a list -> bool
*)
