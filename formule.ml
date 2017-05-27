(* Types *)

type formula =
    |Var of int
    |True
    |False
    |Not of formula
    |And of formula * formula
    |Or of formula * formula
    |Imp of formula * formula
    |Equi of formula * formula
;;

module Read =
    struct

        type grammaire =
            | Entier of int
            | Parenthese_ouv
            | Parenthese_ferm
            | Vrai
            | Faux
            | Non
            | Et
            | Ou
            | Impli
            | Equ
        ;;

        let print = function
            | Entier(x) -> string_of_int x
            | Parenthese_ouv -> "("
            | Parenthese_ferm -> ")"
            | Vrai -> "true"
            | Faux -> "false"
            | Non -> "~"
            | Et -> "&&"
            | Ou -> "||"
            | Impli -> "->"
            | Equ -> "<->"
        ;;
        
        let rec display = function
            | [] -> ()
            | t::q -> print_string (print t); display q
        ;;

        let simplify s =
            let r_space = Str.regexp " " in
            let r_true = Str.regexp "true" in
            let r_false = Str.regexp "false" in
            let r_and = Str.regexp "&&" in
            let r_or = Str.regexp "||" in
            let r_equi = Str.regexp "<->" in
            let r_imp = Str.regexp "->" in
            let a = Str.global_replace r_space "" s in
            let b = Str.global_replace r_true "t" a in
            let c = Str.global_replace r_false "f" b in
            let d = Str.global_replace r_and "&" c in
            let e = Str.global_replace r_or "|" d in
            let f = Str.global_replace r_equi "=" e in
            Str.global_replace r_imp ">" f
        ;;
        
        let cut s =
            let n = String.length s in
            let rec aux i current_number =
                if i = n then
                    if current_number != 0 then
                        [Entier(current_number)]
                    else
                        []
                else
                    let v = (int_of_char s.[i]) - 48 in
                    if v >= 0 && v <= 9 then
                        aux (i + 1) (current_number * 10 + v)
                    else
                        let a = if current_number != 0 then [Entier(current_number)] else [] in
                        let d =
                        match s.[i] with
                        | '(' -> Parenthese_ouv
                        | ')' -> Parenthese_ferm
                        | 't' -> Vrai
                        | 'f' -> Faux
                        | '~' -> Non
                        | '&' -> Et
                        | '|' -> Ou
                        | '>' -> Impli
                        | '=' -> Equ
                        | _ -> Entier(-1) (* ERROR *)
                        in
                        a@(d::(aux (i + 1) 0))
            in
            aux 0 0
        ;;

        let rec eval_equi (u: grammaire list) : formula  * grammaire list =
            let (a, q1) = eval_imp u in
            match q1 with
            | [] -> (a, [])
            | Equ::q2 -> 
                let (b, q3) = eval_equi q2
                in (Equi(a, b), q3)
            | _ -> (a, q1)
        and eval_imp u =
            let (a, q1) = eval_dis u in
            match q1 with
            | [] -> (a, [])
            | Impli::q2 -> 
                let (b, q3) = eval_imp q2
                in (Imp(a, b), q3)
            | _ -> (a, q1)
        and eval_dis u =
            let (a, q1) = eval_conj u in
            match q1 with
            | [] -> (a, [])
            | Ou::q2 -> 
                let (b, q3) = eval_dis q2
                in (Or(a, b), q3)
            | _ -> (a, q1)
        and eval_conj u =
            let (a, q1) = eval_neg u in
            match q1 with
            | [] -> (a, [])
            | Et::q2 -> 
                let (b, q3) = eval_dis q2
                in (And(a, b), q3)
            | _ -> (a, q1)
        and eval_neg = function
            | [] -> (Var(-5), []) (* ERROR *)
            | Parenthese_ouv::q -> let (a, v) = eval_equi q in (a, List.tl v)
            | Entier(x)::q -> (Var(x), q)
            | Vrai::q -> (True, q)
            | Faux::q -> (False, q)
            | Non::q -> let (a, v) = eval_neg q in (Not a, v)
            | _ -> (Var(-6), [])
        ;;
        
        let read s =
            cut (simplify s)
        ;;

        let eval s =
            fst (eval_equi (read s))
        ;;

    end
;;

