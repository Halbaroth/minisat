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

let main timeout log_lvl inputs =
  Logs.set_reporter (reporter Format.err_formatter);
  Logs.set_level (Some log_lvl);
  List.iter (fun input ->
    let (module SAT) = 
      Solver.of_dimacs_file ~timeout ~order:(module Order) input 
    in
    let ans = SAT.check [] in 
    Logs.app (fun k -> k"%a@." pp_answer ans)) inputs

module Cmd = struct
  open Cmdliner

  let timeout =
    let doc = "Timeout" in 
    Arg.(value & opt int 0 & info [ "time" ] ~docv:"TIME" ~doc)

  let log_lvl =
    let doc = "Set the logging level." in
    let level =
      Arg.enum
        [
          ("info",    Logs.Info);
          ("warning", Logs.Warning);
          ("error",   Logs.Error);
          ("debug",   Logs.Debug);
        ]
    in
    Arg.(value & opt level Logs.Info & info [ "d"; "debug" ] ~docv:"LOG_LEVEL" ~doc)

  let inputs =
    Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES")

  let parse () =
    let cmd =
      let doc = "A OCaml implementation of MiniSAT" in
      let man = [
          `S Manpage.s_description;
          `P "Yet another MiniSAT implementation.";
          `S Manpage.s_bugs;
          `P "Bug reports to <pierre.villemot@ocamlpro.com>";
        ]
      in
      let info = Cmd.info "minisat" ~version:"dev" ~doc ~man in
      Cmd.v info Term.(const main $ timeout $ log_lvl $ inputs)
    in
    exit (Cmd.eval cmd)
end

let () = Cmd.parse ()
