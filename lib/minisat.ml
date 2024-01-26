include Lbool

type 'w var = Solver.var
type 'w lit = Solver.lit

module Var = Solver.Var 
module Lit = Solver.Lit

module type Var_order = sig
  type 'w t 

  val make : unit -> 'w t
  val new_var : 'w t -> 'w var -> unit
  val update_var : 'w t -> 'w var -> unit
  val update_all : 'w t -> unit
  val undo : 'w t -> 'w var -> unit
  val select : 'w t -> 'w var 
end

module Solver = struct
  module type S = sig
    type wit

    val new_var : unit -> wit lit
    val add_clause : wit lit list -> bool
    val add_cnf : Dimacs.Ast.t -> unit
    val simplify_db : unit -> bool
    val check : wit lit list -> wit lit answer
  end

  type order = (module Var_order)
  type t = (module S)

  let make ?timeout ~order:(module O : Var_order) () =
    let module WIT = (struct type t end) in
    let module SAT = Solver.Make 
      (struct 
        include O
        type nonrec t = WIT.t t 
      end)
    in 
    let env = SAT.make ?timeout () in
    (module struct 
      type wit = WIT.t 

      let new_var () = SAT.new_var env 
      let add_clause = SAT.add_clause env 
      let add_cnf = SAT.add_cnf env
      let simplify_db () = SAT.simplify_db env 
      let check = SAT.check env
    end : S)

  (* TODO: move this function in the Dimacs project *)
  let pp_position fmt (pos : Lexing.position) =
    Fmt.pf fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)

  let of_dimacs_file ?timeout ~order filename =
    let open Dimacs in
    let (module SAT) = make ?timeout ~order () in 
    let oi = open_in filename in
    let lexbuf = Lexing.from_channel ~with_positions:true oi in
    Fun.protect ~finally:(fun () -> close_in_noerr oi) @@ fun () ->
      try
        let cnf = Parser.cnf Lexer.read lexbuf in
        SAT.add_cnf cnf;
        (module SAT : S)
      with Parser.Error as exn ->
        let bt = Printexc.get_raw_backtrace() in
        Logs.debug (fun k -> k"syntax error at position %a"
          pp_position lexbuf.lex_curr_p);
        Printexc.raise_with_backtrace exn bt
end 

module Vec = Vec
