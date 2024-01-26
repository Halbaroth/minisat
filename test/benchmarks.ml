open Minisat

module Order = struct
  type 'a t = 'a var Vec.t

  let make () = Vec.make ~dummy:Var.dummy 17 
  let new_var env v = Vec.push env v
  let update_var _ _ = ()
  let update_all _ = ()
  let undo _ _ = ()
  let select env =
    Vec.find_first 
      (fun v -> equal_lbool (Var.value v) Unknown) env
end

let reporter ppf =
  let report _src level ~over k msgf =
    let k _ = over (); k () in
    let with_header h _tags k ppf fmt =
      Format.kfprintf k ppf ("%a @[" ^^ fmt ^^ "@]@.") Logs.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_header header tags k ppf fmt
  in
  { Logs.report = report }

let check_sat filename =
  try
    let (module SAT) = 
      Solver.of_dimacs_file ~order:(module Order) filename
    in
    let ans = SAT.check [] in
    Logs.app (fun k -> k"%s: %a" 
      (Filename.basename filename) 
      pp_answer ans)
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Logs.err (fun k -> k"failed %s" filename);
    Printexc.raise_with_backtrace exn bt

let () =
  Logs.set_reporter (reporter Format.err_formatter);
  Logs.set_level (Some Logs.Error);
  let dir = Sites.Sites.satlib |> List.hd in
  let tests = Sys.readdir dir in
  Array.iter (fun name -> check_sat (Filename.concat dir name)) tests
