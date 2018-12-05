open CCList.Infix

let l = 1 -- 50

let rec print_list printer = function
  | [] -> ()
  | h::t -> printer h; print_list printer t

let p x = print_int x; print_char ' '

let () = print_list p l; print_newline ()
