let g = open_in "./input.txt"|> CCIO.read_lines

let lines = 
  let rec read gen =
    match Gen.next gen with
    | None -> []
    | Some x -> x :: read gen
  in
  read g

let vals = CCList.map int_of_string lines 

let result = vals |> CCList.fold_left (+) 0

(* let _ = CCList.iter (fun x -> print_int x; print_char ' ') vals; print_newline () *)

let _ = print_int result; print_newline ()

module IntSet = CCSet.Make (CCInt)

let rec find_repeat l = 
  let rec find s sum = function
    | [] -> find s sum l
    | h::t -> 
      let sum' = h + sum in
      if IntSet.mem sum' s then Some sum' else find (IntSet.add sum' s) sum' t
  in
  find IntSet.empty 0 l

let x = match find_repeat vals with
  | None -> failwith "impossible"
  | Some x -> x

let _ = print_int x; print_newline ()


