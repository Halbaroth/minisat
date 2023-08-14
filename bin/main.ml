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

let main log_lvl =
  Logs.set_reporter (reporter Format.err_formatter);
  Logs.set_level (Some log_lvl);
  Fmt.pr "@.";
  let open Solver.Syntax in
  let solver = SAT.make () in
  let x = SAT.new_var solver in
  let y = SAT.new_var solver in
  let _ =
    let b1 = SAT.add_clause solver [lit x; lit y] in
    let b2 = SAT.add_clause solver [neg (lit x); neg (lit y)] in
    let b3 = SAT.add_clause solver [neg (lit x); lit y] in
    let b4 = SAT.add_clause solver [lit x; neg (lit y)] in
    assert (b4);
  in
  match SAT.solve solver [] with
  | Sat _ -> Fmt.pr "SAT@."
  | Unsat -> Fmt.pr "UNSAT@."

module Cmd = struct
  open Cmdliner

  let log_lvl =
    let doc = "Set the logging level." in
    let level =
      Arg.enum
        [
          ("info", Logs.Info);
          ("warning", Logs.Warning);
          ("error", Logs.Error);
          ("debug", Logs.Debug);
        ]
    in
    Arg.(value & opt level Logs.Info & info [ "d"; "debug" ] ~docv:"LOG_LEVEL" ~doc)

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
      Cmd.v info Term.(const main $ log_lvl)
    in
    exit (Cmd.eval cmd)
end

let () = Cmd.parse ()

