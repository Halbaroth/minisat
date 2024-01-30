open Minisat

let is_unknown v = Core.equal_lbool (Var.value v) Unknown

(* module Order = struct
  type 'a t = 'a var Vec.t

  let make () = Vec.make ~dummy:Var.dummy 17 
  let new_var env v = Vec.push env v
  let update_var _ _ = ()
  let update_all _ = ()
  let undo _ _ = ()
  let select = Vec.find_first is_unknown 
end
*)
module Order = struct
  type 'a t = 'a var Vec.t

  let make () = Vec.make ~dummy:Var.dummy 17 
  let new_var env v = Vec.push env v
  let update_var _ _ = ()
  let update_all _ = ()
  let undo _ _ = ()
  let select (env : 'a t) =
    let first = Vec.find_first is_unknown env in
    Vec.fold 
      (fun max v ->
        if is_unknown v && Var.activity v > Var.activity max 
        then v 
        else max
      ) first env
end

let reporter ppf =
  Fmt.set_style_renderer ppf `Ansi_tty;
  let report _src level ~over k msgf =
    let k _ = over (); k () in
    let with_header h _tags k ppf fmt =
      Fmt.kpf k ppf ("%a @[" ^^ fmt ^^ "@]@.") Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_header header tags k ppf fmt
  in
  { Logs.report }

type answer = 
  | Sat of float 
  | Unsat of float
  | Timeout
  | Error of exn

let pp_timer ppf time = Fmt.pf ppf "(%.2f ms)" (time *. 1000.)

let pp_answer ppf ans =
  match ans with 
  | Sat time -> 
    Fmt.(
      (const string "sat" |> styled (`Fg `Green)) 
      ++ sp ++ (const pp_timer time)
    ) ppf ()

  | Unsat time -> 
    Fmt.(
      (const string "unsat" |> styled (`Fg `Blue)) 
      ++ sp ++ (const pp_timer time)
    ) ppf ()

  | Timeout -> 
    Fmt.(const string "timeout" |> styled (`Fg `Yellow)) ppf ()

  | Error e ->
    Fmt.(
      (const string "failed with the exception" |> styled (`Fg `Red)) 
      ++ sp ++ (const exn e)
    ) ppf ()

let check_sat filename =
  try
    let (module SAT) = 
      Solver.of_dimacs_file ~order:(module Order) filename
    in
    Core.with_timeout ~timelimit:1 @@ fun () ->
      let time, ans = Core.with_timer @@ fun () -> SAT.check [] in
      match ans with 
      | Core.Sat _ -> Sat time 
      | Unsat -> Unsat time 
  with 
  | Core.Timeout -> Timeout
  | exn -> Error exn

module B = PrintBox 

let text color s = 
  B.(text_with_style color s |> align ~h:`Center ~v:`Center)

let default_text = text B.Style.default
let green_text = text B.Style.(fg_color Green)
let blue_text = text B.Style.(fg_color Blue)
let red_text = text B.Style.(fg_color Red)
let yellow_text = text B.Style.(fg_color Yellow)

let () =
  Logs.set_reporter (reporter Fmt.stdout);
  Logs.set_level (Some Logs.Error);
  let dir = Sites.Sites.satlib |> List.hd in
  let tests = Sys.readdir dir in
  let n_sat, n_unsat, n_timeout, n_error, total_time = 
    Array.fold_left 
      (fun (n_sat, n_unsat, n_timeout, n_error, total_time) name -> 
        let res = check_sat (Filename.concat dir name) in
        Logs.app (fun k -> k"%a (...%s)" pp_answer res name);
        match res with 
        | Sat time -> 
          (n_sat + 1, n_unsat, n_timeout, n_error, total_time +. time)
        | Unsat time -> 
          (n_sat, n_unsat + 1, n_timeout, n_error,  total_time +. time)
        | Timeout -> 
          (n_sat, n_unsat, n_timeout + 1, n_error, total_time)
        | Error _ -> 
          (n_sat, n_unsat, n_timeout, n_error + 1, total_time)
      ) (0, 0, 0, 0, 0.) tests
  in
  let module B = PrintBox in
  let box = PrintBox.grid
    [|
      [| green_text "sat";
         blue_text "unsat"; 
         yellow_text "timeout"; 
         red_text "error"; 
         default_text "total time" 
      |];
      [| string_of_int n_sat |> default_text; 
         string_of_int n_unsat |> default_text; 
         string_of_int n_timeout |> default_text; 
         string_of_int n_error |> default_text;
         Fmt.str "%.2f s" total_time |> default_text 
      |]
    |]
  in
  Logs.app (fun k -> k"%a" PrintBox_text.pp box)
