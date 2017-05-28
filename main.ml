include Tetravex
  

let readString n =
  let rec help q i=
    match i with
    |1 -> q
    |i -> help (Sys.argv.(i)^q) (i-1)
  in help "" n

let readTetravex n =
  let rec help q i j =
    match i with
    |0 -> q
    |i -> help ((new Tetravex.domino (int_of_string Sys.argv.(4*i)) (int_of_string Sys.argv.(4*i+1)) (int_of_string Sys.argv.(4*i+2)) (int_of_string Sys.argv.(4*i+3)) j)::q) (i-1) (j+1)
            in help [] n 0
    
let () =
  let test_type = Sys.argv.(1) in
  if test_type = "dump"
  then let n =  (Array.length Sys.argv)
    in let s = readString (n-1) 
    in print_string s;
    let bdd = IntBDD.createBDD (ReadFormule.eval s)
    in IntBDD.print bdd
  else if test_type = "valid"
  then let n =  (Array.length Sys.argv)
    in let s = readString (n-1) 
    in print_string s;
    let bdd = IntBDD.createBDD (ReadFormule.eval s)
    in
    if  IntBDD.isValid bdd then print_int 0; 
  else if test_type = "satisfiable"
  then let n =  (Array.length Sys.argv)
    in let s = readString (n-1) 
    in print_string s;
    let bdd = IntBDD.createBDD (ReadFormule.eval s)
    in if  IntBDD.isSatisfiable bdd then print_int 0;
  else if test_type = "tetravex"
  then
    let n = int_of_string Sys.argv.(2) in
    let p = int_of_string Sys.argv.(3) in
    let x = Array.length Sys.argv in
    let q = readTetravex (x/4)
    in print_int n
          
(*      
    else
    let s = "1^2"
    in let a =  ReadFormule.eval s
    in let b = IntBDD.createBDD a
           in IntBDD.print b
*)

