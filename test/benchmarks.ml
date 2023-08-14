open Minisat

module Order = struct
  let new_var _ = ()
  let update_var _ _ = ()
  let update_all _ = ()
  let undo _ _ = ()
  let select env =
    Vec.find_first ~p:(fun v -> Solver.value v = Unknown) env
end

module SAT = Solver.Make (Order)

let reporter ppf =
  let report _src level ~over k msgf =
    let k _ = over (); k () in
    let with_header h _tags k ppf fmt =
      Format.kfprintf k ppf ("%a @[" ^^ fmt ^^ "@]@.") Logs.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_header header tags k ppf fmt
  in
  { Logs.report = report }

let print_answer fmt ans =
  let open Solver in
  match ans with
  | Sat _ -> Fmt.pf fmt "sat"
  | Unsat -> Fmt.pf fmt "unsat"

let check_sat filename =
  try
    let solve = SAT.of_dimacs_file filename in
    let ans = SAT.solve solve [] in
    Logs.app (fun k -> k "%s: %a" (Filename.basename filename) print_answer ans)
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Fmt.pr "Failed %s@." filename;
    Printexc.raise_with_backtrace exn bt


let () =
  Logs.set_reporter (reporter Format.err_formatter);
  Logs.set_level (Some Logs.Error);
  let dir = Sites.Sites.satlib |> List.hd in
  let tests = Sys.readdir dir in
  Array.iter (fun name -> check_sat (Filename.concat dir name)) tests

(* open Satanas

module A = struct
  type t = int

  (* We exclude zero since 0 = -0 and an atom and its negation
     must always be different! *)
  let counter = ref 1

  let neg t = -t
  let compare = Int.compare
  let equal = Int.equal

  let fresh () =
    if !counter < Int.max_int then
      let res = !counter in
      incr counter;
      res
    else
      failwith "fresh: maximum variable reached"

  let pp fmt a =
    if a >= 0 then
      Format.fprintf fmt "X_%i" a
    else
      Format.fprintf fmt "¬X_%i" (Int.abs a)
end

module Sat = Solver.Make (A)

let print_answer fmt ans =
  let open Solver in
  match ans with
  | Sat _ -> Format.fprintf fmt "sat"
  | Unsat _ -> Format.fprintf fmt "unsat"

let check_sat filename =
  let cnf = Cnf.of_dimacs_file filename in
  Logs.app (fun k -> k "%s: %a" (Filename.basename filename)
    print_answer (Sat.solve cnf))

let reporter ppf =
  let report _src level ~over k msgf =
    let k _ = over (); k () in
    let with_header h _tags k ppf fmt =
      Format.kfprintf k ppf ("%a @[" ^^ fmt ^^ "@]@.") Logs.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_header header tags k ppf fmt
  in
  { Logs.report = report }

let () =
  Logs.set_reporter (reporter Format.err_formatter);
  Logs.set_level (Some Logs.Debug);
  let dir = Sites.Sites.satlib |> List.hd in
(*   let tests = Sys.readdir dir in *)
  (* Array.iter (fun name -> check_sat (Filename.concat dir name)) tests *)
  check_sat (Filename.concat dir "uf20-059.cnf") *)
