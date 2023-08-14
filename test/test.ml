open Satanas

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
  check_sat (Filename.concat dir "uf20-059.cnf")
