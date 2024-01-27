type lbool = True | False | Unknown [@@deriving show, compare, equal]

let[@inline always] neg_lbool b =
  match b with
  | True -> False
  | False -> True
  | Unknown -> Unknown

type 'a answer =
  | Sat of 'a array 
  | Unsat
  | Timeout
  (** The time limit is reached. *)

let pp_answer ppf ans =
  match ans with 
  | Sat _ -> Fmt.pf ppf "sat"
  | Unsat -> Fmt.pf ppf "unsat"
  | Timeout -> Fmt.pf ppf "timeout"
