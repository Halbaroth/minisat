type lbool = True | False | Unknown [@@deriving show, compare, equal]

let[@inline always] neg_lbool b =
  match b with
  | True -> False
  | False -> True
  | Unknown -> Unknown

type 'a answer =
  | Sat of 'a array 
  | Unsat

let pp_answer ppf ans =
  match ans with 
  | Sat _ -> 
    Fmt.(const string "sat" |> styled (`Fg `Green)) ppf ()
  | Unsat -> 
    Fmt.(const string "unsat" |> styled (`Fg `Blue)) ppf () 

exception Timeout 

let with_timeout =
  let sigalrm_handle signal = 
    if signal = Sys.sigalrm then
      raise Timeout 
  and finally () = ignore(Unix.alarm 0) 
  in fun ~timelimit f ->
    if timelimit <= 0 then f ()
    else 
      Fun.protect ~finally @@ fun () ->
        Sys.(set_signal sigalrm (Signal_handle sigalrm_handle));
        ignore(Unix.alarm timelimit);
        f ()

let with_timer f =
  let start = Sys.time () in
  let res = f () in
  Sys.time () -. start, res
