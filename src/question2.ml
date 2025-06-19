(** CW2, QUESTION 2 **)

(* a (3 marks) *)
(* let extractMin xs = failwith "Not implemented" *)

let rec rev_append list1 list2 =
  match list1 with
  | [] -> list2
  | hd :: tl -> rev_append tl (hd :: list2) (* function for putting two lists together but the first one in reversed order*)

let rec rev list =
  match list with
  | [] -> []
  | hd :: tl -> rev_append (rev tl) [hd](* just function for reversing list for using later*)


let rec extractMin lst =
  let rec find_min min acc xs =
    match xs with
    |[] -> (min, rev_append acc xs) 
    |hd :: tl ->    (*using here binary sort for making less iterations*)
      if hd < min          (* checking condition bigger or lesser what we expect*)
        then find_min hd (hd :: acc) tl
        else find_min min (hd :: acc) tl
          in
          match lst with
          |[] -> failwith "Empty list"
          |hd :: tl -> let (min, rest) = (* making a storage with value and list with rest of the valuse how it was in example *)
                      find_min hd [] tl in
                      (min, rest);;




(* b (3 marks) *)
(*let extractSort xs = failwith "Not implemented"*)

let rec extractSort lst =
  let rec extractMin xs =
    match xs with
    |[] -> failwith "Empty list" (*3 cases how it can be *)
    |[x] -> (x, [])
    |x :: rest ->
      let (min, rest') = extractMin rest in (*using recurtion sorting list relying on a min value*)
      if x < min then (x, rest) else (min, x :: rest') in
    let rec sort acc xs =
    match xs with
    |[] -> acc
    |_ ->
      let (min, rest) = extractMin xs in
      sort (min :: acc) rest in sort [] lst;;

(* c (2 marks) *)
(* Provide your answer in this comment block

We have the input list [2; 3; 1] lets call it il[] and an empty result list [] lets call it rl[]
We find the minimum element in the il, which is number 1 in that case and add 1 to the rl[] and remove it from the il[ ] .
Now, our rl is [1], and the il is [2; 3] because we did previous steps
We repeat the process until we sort all valuse in il[]  find the minimum in the remaining il[], which is 2, add 2 to the rl[] and remove it from the il[].
Our result list is now [1; 2], and the input list is [3]
And we doing that until the il[] becomes empty list as if first line in match constrution
in our case we should do that one more time because we have one value left 
the rl is [1; 2; 3], and the il[] is now empty
We're done because there are no elements left in the il
The sorted result is [1; 2; 3], which is the original list sorted in ascending order
 *)

(* d (2 marks) *)
(* Provide your answer in this comment block

extractMin 

1)no the extractMin function is not tail-recursive because it makes a recursive call to itself
and then performs additional work after the recursive call returns
2) y es, the function generates some garbage due to the creation of intermediate lists while processing elements

extractSort

1) No, the extractSort function is not tail-recursive because it makes recursive calls to itself
and performs additional work after each recursive call.Like in first func
2) yes, the function generates some garbage, especially in case when creating new lists while building the sorted result

In both cases functions are not tail-recursive because they do not have the recursive call as the last operation in their process
and both functions generate some garbage when creating new lists or making intermediate list modifications during execution and thats makes them alook like 
