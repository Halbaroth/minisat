open Ppx_compare_lib.Builtin

type lbool = True | False | Unknown [@@deriving show, compare, equal]

let[@inline always] neg_lbool b =
  match b with
  | True -> False
  | False -> True
  | Unknown -> Unknown

type lit = {
  var : var;
  sign : bool;
  watched : clause Vec.t;
  neg : lit;
}
and clause = {
  learnt : bool;
  mutable cla_activity : float;
  mutable lits : lit array
}
and var = {
  mutable var_activity : float;
  undos : clause Vec.t;
  mutable dlvl : int;
  mutable reason : clause;
  mutable value : lbool;
  lit : lit;
  id : int;
  dimacs_id : int option;
  (* Original id if the variable have been added from 
     a DIMACS file. Use for debugging purpose. *)
  (* BUG: If we add both DIMACS clauses and clauses through the 
     API, we could get the same id. *)
}

let rec compare_lit lit1 lit2 =
  if lit1 == lit2 then 0
  else
    let c = Bool.compare lit1.sign lit2.sign in
    if c <> 0 then c
    else
      compare_var lit1.var lit2.var

and compare_clause cla1 cla2 =
  if cla1 == cla2 then 0
  else
    compare_array compare_lit cla1.lits cla2.lits

and compare_var { id = id1; _ } { id = id2; _ } = id1 - id2

let[@inline always] value b = b.value

(* TODO: move this in the module clause. *)
(* Dummy value used by vectors of clauses. *)
let dummy_clause = {
  learnt = false;
  cla_activity = 0.;
  lits = [||];
}

let rec dummy_var = {
  var_activity = 0.;
  undos = Vec.make ~dummy:dummy_clause 0;
  dlvl = -1;
  reason = dummy_clause;
  value = Unknown;
  lit = dummy_lit;
  id = -1;
  dimacs_id = None;
}
and dummy_lit = {
  var = dummy_var;
  sign = true;
  watched = Vec.make ~dummy:dummy_clause 0;
  neg = neg_dummy_lit
}
and neg_dummy_lit = {
  var = dummy_var;
  sign = false;
  watched = Vec.make ~dummy:dummy_clause 0;
  neg = dummy_lit
}

let pp_var ppf ({ id; dimacs_id; _ } as var) =
  if var == dummy_var then
    Fmt.pf ppf "(dummy_var)"
  else
    let id =
      match dimacs_id with
      | Some id -> id
      | None -> id
    in
    Fmt.int ppf id

let pp_lit ppf ({ var; sign; _ } as lit) =
  if lit == dummy_lit then
    Fmt.pf ppf "(dummy_lit)"
  else
    if sign then pp_var ppf var
    else Fmt.pf ppf "-%a" pp_var var

let show_lit = Fmt.str "%a" pp_lit

let pp_clause ppf ({ lits; _ } as clause) =
  if clause == dummy_clause then
    Fmt.pf ppf "(dummy_clause)"
  else
    Fmt.(array ~sep:sp pp_lit ppf lits)

let show_clause = Fmt.str "%a" pp_clause

let show_var = Fmt.str "%a" pp_var

module Syntax = struct
  let[@inline always] neg lit = lit.neg
  let[@inline always] lit var = var.lit
end

module type Var_order = sig
  type env

  val new_var : env -> unit
  val update_var : env -> var -> unit
  val update_all : env -> unit
  val undo : env -> var -> unit
  val select : env -> var
end

type answer =
  | Sat of bool array
  | Unsat

module type S = sig
  type t

  val make : unit -> t
  val of_dimacs_file : string -> t
  val new_var : t -> var
  val add_clause : t -> lit list -> bool
  val simplify_db : t -> bool
  val solve : t -> lit list -> answer
end

module Lit : sig
  type t = lit

  val value : t -> lbool
  val compare : t -> t -> int
  module Set : Set.S with type elt = t
end = struct
  type t = lit

  let[@inline always] value { var; sign; _ } =
    if sign then neg_lbool var.value else var.value

  let compare = compare_lit

  module Set = Set.Make (struct
    type t = lit
    let compare = compare_lit
  end)
end

module Clause : sig
(*   val locked : clause -> bool *)
  (* Check if a learnt clause is locked, that is the clause is the reason
     of some propagation. *)

  val remove : clause -> unit
  (** Remove the clause from the watch lists. *)

  val simplify : clause -> bool
  (** Simplify a clause by removing all its false literals. This
      function can be called only at top-level with an empty propagation
      queue. *)

  val calc_reason : clause -> lit -> lit Vec.t

  val undo : clause -> lit -> unit
end = struct
  let calc_reason { lits; _ } p =
    assert (p == dummy_lit || p == lits.(0));
    let j = if p == dummy_lit then 0 else 1 in
    let size = Array.length lits in
    let reason = Vec.make ~dummy:dummy_lit (size-j) in
    for i = j to size - 1 do
      assert (Lit.value lits.(i) == False);
      Vec.push reason lits.(i).neg
    done;
    reason

  (* let locked ({lits; learnt; _ } as clause) =
    assert learnt;
    compare_clause lits.(0).var.reason clause = 0 *)

  let remove ({lits; _} as clause) =
    Vec.remove lits.(0).neg.watched clause;
    Vec.remove lits.(1).neg.watched clause

  let simplify clause =
    let lits = clause.lits in
    let j = ref 0 in
    try
      for i = 0 to Array.length lits - 1 do
        match Lit.value lits.(i) with
        | True -> raise Exit
        | Unknown ->
            lits.(!j) <- lits.(i);
            incr j
        | False -> ()
      done;
      let l = Array.length lits - !j in
      clause.lits <- Array.init l (fun i -> lits.(i));
      false
    with Exit -> true

  let undo _ _ = ()
end

module Make (O : Var_order with type env := var Vec.t) = struct
  type t = {
    mutable root_lvl : int;

    (* Constraint database. *)
    constrs : clause Vec.t;
    learnts : clause Vec.t;
    mutable cla_inc : float;
    mutable cla_decay : float;

    (* Variable order. *)
    mutable var_inc : float;
    mutable var_decay : float;

    (* Propagation queue. *)
    propagations: lit Queue.t;

    (* Assignments. *)
    vars: var Vec.t;
    trail: lit Vec.t;
    trail_lim: int Vec.t;
  }

  let[@inline always] nb_vars env = Vec.size env.vars
  let[@inline always] nb_assigns env = Vec.size env.trail
  let[@inline always] nb_constrs env = Vec.size env.constrs
(*   let[@inline always] nb_learnts env = Vec.size env.learnts *)
  let[@inline always] decision_level env = Vec.size env.trail_lim

  let var_rescale_activity env =
    Vec.iter 
      (fun var -> var.var_activity <- var.var_activity *. 1e-100) env.vars;
    env.var_inc <- env.var_inc *. 1e-100

  let var_bump_activity env var =
    var.var_activity <- var.var_activity +. env.var_inc;
    let _ =
      if var.var_activity > 1e100 then
      var_rescale_activity env
    in
    O.update_var env.vars var

  let var_decay_activity env =
    env.var_inc <- env.var_inc *. env.var_decay

  (* let cla_rescale_activity env =
    (* TODO: Check if we have to rescale the activity of constraints also? *)
    Vec.iter env.learnts
      ~f:(fun cla -> cla.cla_activity <- cla.cla_activity *. 1e-100);
    env.cla_inc <- env.cla_inc *. 1e-100 *)

  let cla_bump_activity env cla =
    cla.cla_activity <- cla.cla_activity +. env.cla_inc;
    let _ =
      if cla.cla_activity > 1e100 then
      var_rescale_activity env
    in
    ()
    (* TODO: Do we need a continuation for cla_bump_activity? *)
(*     O.update_var env.vars var *)

  let cla_decay_activity env =
    env.cla_inc <- env.cla_inc *. env.cla_decay

  let decay_activities env =
    var_decay_activity env;
    cla_decay_activity env

  (* Find a literal with the maximal decision level in the array ps. *)
  let highest_dlvl ps =
    Vec.fold (fun (i, max) lit ->
      if lit.var.dlvl > max then
        (i+1, lit.var.dlvl)
      else
        (i+1, max)
    ) (-1, -1) ps
    |> fst

  (* Remove the duplicates and false literals. *)
  let normalize ps =
    (* TODO: create a to_seq operation in Vec module. *)
    Vec.to_array ps
    |> Array.to_seq
    |> Lit.Set.of_seq
    |> Lit.Set.filter (fun lit ->
        compare_lbool (Lit.value lit) False <> 0)
    |> Lit.Set.to_seq
    |> Array.of_seq
    |> Vec.of_array ~dummy:dummy_lit

  let enqueue env ?(from = dummy_clause) p =
    match Lit.value p with
    | False -> false
    | True -> true
    | Unknown ->
        Logs.debug (fun k -> k"enqueue literal %a with reason %a" pp_lit
          p pp_clause from);
        (* Store the new fact. *)
        let _ =
          if p.sign then
            p.var.value <- False
          else
            p.var.value <- True
        in
        p.var.dlvl <- decision_level env;
        p.var.reason <- from;
        Vec.push env.trail p;
        Queue.push p env.propagations;
        Logs.debug (fun k -> k"assignment after enqueue %a"
          Fmt.(iter ~sep:sp Vec.iter pp_lit)
          env.trail);
        true

  let propagate_in_clause env clause p =
    Logs.debug (fun k -> k"propagate literal %a in clause %a" pp_lit p
      pp_clause clause);
    let lits = clause.lits in
    assert (Array.length lits >= 2);
    (* Make sure the false literal is lits.(1). *)
    if Lit.compare lits.(0) p.neg = 0 then begin
      lits.(0) <- lits.(1);
      lits.(1) <- p.neg
    end;
    (* If the first literal is true, the clause is already satisfied. *)
    if equal_lbool (Lit.value lits.(0)) True then begin
      Vec.push p.watched clause;
      assert (Vec.size p.watched > 0);
      true
    end
    else
      (* Look for a new literal to watch. *)
      try
        for i = 2 to Array.length lits - 1 do
          if not @@ equal_lbool (Lit.value lits.(i)) False then begin
            lits.(1) <- lits.(i);
            lits.(i) <- p.neg;
            Vec.push lits.(1).neg.watched clause;
            assert (Vec.size lits.(1).neg.watched > 0);
            raise Exit
          end
        done;
        (* We don't find any literal to watch, which means the clause 
           is unit under the current assignment and we have 
           to propagate [lits.(0)] to satisfy it. *)
        Vec.push p.watched clause;
        assert (Vec.size p.watched > 0);
        enqueue env lits.(0) ~from:clause
      with Exit -> true

  let new_clause env ps ~learnt =
    let aux ps =
      let lits = Vec.to_array ps in
      let clause = {
        learnt;
        cla_activity = 0.;
        lits
      }
      in
      match Array.length lits with
      | 0 -> false, None
      | 1 -> enqueue env lits.(0), None
      | _ ->
        let () =
          if learnt then begin
            (* Pick a second literal to watch. *)
            let max_i = highest_dlvl ps in
            assert (max_i >= 0);
            lits.(1) <- Vec.get ps max_i;
            lits.(max_i) <- Vec.get ps 1;
            (* Bumping the activity of the clause. *)
            cla_bump_activity env clause;
            Vec.iter (fun lit -> var_bump_activity env lit.var) ps
          end
        in
        (* Add clause to watcher vectors. *)
        Vec.push lits.(0).neg.watched clause;
        Vec.push lits.(1).neg.watched clause;
        true, Some clause
    in
    if not learnt then
      if Vec.exists (fun lit -> equal_lbool (Lit.value lit) True) ps then
        true, None
      else if Vec.exists (fun lit -> Vec.mem lit.neg ps) ps then
        true, None
      else
        aux (normalize ps)
    else
      aux ps

  let add_clause env ps =
    let ps = Vec.of_list ~dummy:dummy_lit ps in
    match new_clause env ps ~learnt:false with
    | res, Some c ->
        Vec.push env.constrs c;
        Logs.debug (fun k -> k"add clause %a" 
          Fmt.(iter ~sep:sp Vec.iter pp_lit) ps);
        res
    | res, None ->
        res

  let record env lits =
    Logs.debug (fun k -> k"record the clause %a" 
        Fmt.(iter ~sep:sp Vec.iter pp_lit) lits);
    assert (Vec.size lits > 0);
    let res, clause = new_clause env lits ~learnt:true in
    assert res;
    let res = enqueue env ?from:clause (Vec.get lits 0) in
    assert res;
    match clause with
    | Some c -> Vec.push env.learnts c
    | None -> ()

  let assume env p =
    assert (Queue.is_empty env.propagations);
    let lim = nb_assigns env in
    Logs.debug (fun k -> k"-------------@\n\
      current model: %a" 
      Fmt.(iter ~sep:sp Vec.iter pp_lit) env.trail);
    Vec.push env.trail_lim lim;
    Logs.debug (fun k -> k"assume %a at level %i" pp_lit p
      (decision_level env));
    enqueue env p

  (* Unbind the last variable on the trail. *)
  let undo_one env =
    let p = Vec.last env.trail in
    p.var.value <- Unknown;
    p.var.reason <- dummy_clause;
    p.var.dlvl <- -1;
    O.undo env.vars p.var;
    Vec.pop env.trail;
    while Vec.size p.var.undos > 0 do
      Clause.undo (Vec.last p.var.undos) p;
      Vec.pop p.var.undos
    done

  (* Revert to the state before last push. *)
  let cancel env =
    assert (Queue.is_empty env.propagations);
    let c = ref ((nb_assigns env) - (Vec.last env.trail_lim)) in
    while !c > 0 do
      undo_one env;
      decr c
    done;
    Vec.pop env.trail_lim

  (* Cancel several levels of assumptions. *)
  let cancel_until env lvl =
    while decision_level env > lvl do
      cancel env
    done

  let new_var ~dimacs_id env =
    let rec var = {
      var_activity = 0.;
      undos = Vec.make ~dummy:dummy_clause 5;
      dlvl = -1;
      reason = dummy_clause;
      value = Unknown;
      lit;
      id = nb_vars env;
      dimacs_id
    }
    and lit = {
      var;
      sign = true;
      watched = Vec.make ~dummy:dummy_clause 0;
      neg = neg_lit;
    }
    and neg_lit = {
      var;
      sign = false;
      watched = Vec.make ~dummy:dummy_clause 0;
      neg = lit;
    }
    in
    (* Should we give the new variable as an argument to O.new_var? *)
    Vec.push env.vars var;
    O.new_var env.vars;
    Logs.debug (fun k -> k "new var %a" pp_var var);
    var

  exception Found of clause

  let make () = {
    root_lvl = 0;
    constrs = Vec.make ~dummy:dummy_clause 5;
    learnts = Vec.make ~dummy:dummy_clause 5;
    cla_inc = 0.5;
    cla_decay = -0.3;
    var_inc = 0.5;
    var_decay = -0.3;
    propagations = Queue.create ();
    vars = Vec.make ~dummy:dummy_var 5;
    trail = Vec.make ~dummy:dummy_lit 5;
    trail_lim = Vec.make ~dummy:(-1) 5;
  }

  (* TODO: move this function in the Dimacs project *)
  let pp_position fmt (pos : Lexing.position) =
    Fmt.pf fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)

  module IntMap = Map.Make (Int)

  let add_cnf Dimacs.Loc.{ data = (_ , clauses); _ } =
    let open Dimacs in
    let open Syntax in
    let env = make () in
    let cache : var IntMap.t ref = ref IntMap.empty in
    List.iter (fun Loc.{ data = clause; _ } ->
      let clause =
        List.fold_left (fun acc Loc.{ data = l; _ } ->
          let var =
            match IntMap.find (Int.abs l) !cache with
            | var -> var
            | exception Not_found ->
              let v = new_var ~dimacs_id:(Some (Int.abs l)) env in
              cache := IntMap.add (Int.abs l) v !cache;
              v
          in
          assert (l <> 0);
          let l = if l > 0 then lit var else neg (lit var) in
          l :: acc
        ) [] clause
        |> List.rev
      in
      let _ : bool = add_clause env clause in ()
    ) clauses;
    env

  (* TODO: move this function in the Dimacs project *)
  let of_dimacs_file filename =
    let open Dimacs in
    let lexbuf =
      open_in filename |> Lexing.from_channel ~with_positions:true
    in
    try
      let cnf = Parser.cnf Lexer.read lexbuf in
      add_cnf cnf
    with Parser.Error as exn ->
      let bt = Printexc.get_raw_backtrace() in
      Logs.debug (fun k -> k"syntax error at position %a"
        pp_position lexbuf.lex_curr_p);
      Printexc.raise_with_backtrace exn bt

  let new_var = new_var ~dimacs_id:None

  let propagate env =
    try
      while Queue.length env.propagations > 0 do
        let p = Queue.pop env.propagations in
        Logs.debug (fun k -> k"propagate literal %a" pp_lit p);
        Logs.debug (fun k -> k"literal %a watches clauses: %a" 
          pp_lit p
          Fmt.(iter ~sep:comma Vec.iter pp_clause) p.watched);
        (* TODO: replace the copy by a move as we don't use the 
          old watches vector anymore. *)
        let tmp = Vec.copy p.watched in
        (* FIXME: is it necessary? *)
        Vec.clear p.watched;
        for i = 0 to Vec.size tmp - 1 do
          if not @@ propagate_in_clause env (Vec.get tmp i) p then
            begin
              (* We found a conflict. *)
              for j = i + 1 to Vec.size tmp - 1 do
                Vec.push p.watched (Vec.get tmp j)
              done;
              Queue.clear env.propagations;
              raise_notrace (Found (Vec.get tmp i))
            end
        done
      done;
      None
    with Found c ->
      assert (Array.for_all (fun lit -> Lit.value lit = False) c.lits);
      Some c

  let analyze env confl =
    assert (decision_level env > env.root_lvl);
    Logs.debug (fun k -> k"analyze %a" pp_clause confl);
    let confl = ref confl in
    let seen = Array.init (nb_vars env) (fun _ -> false) in
    let counter = ref 0 in
    let p = ref dummy_lit in
    let bt_lvl = ref 0 in
    let out_learnt = Vec.make ~dummy:dummy_lit 5 in
    (* FIXME: really unsafe. We should'nt use the dummy value there. *)
    Vec.push out_learnt dummy_lit;
    (* TODO: separate the two cases by creating two functions for calc_reason *)
    while !p == dummy_lit || !counter > 0 do
      assert (compare_clause !confl dummy_clause <> 0);
      let p_reason = Clause.calc_reason !confl !p in
      if !confl.learnt then cla_bump_activity env !confl;

      (* Trace reason for the literal [p]. *)
      assert (Vec.size p_reason > 0);
      Vec.iter (fun q ->
        if not seen.(q.var.id) then begin
          seen.(q.var.id) <- true;
          if q.var.dlvl = decision_level env then begin
            incr counter
          end
          else if q.var.dlvl > 0 then begin
            Vec.push out_learnt q.neg;
            bt_lvl := max !bt_lvl q.var.dlvl
          end
        end
      ) p_reason;

      (* Select next literal to look at. *)
      let dowhile = ref true in
      while !dowhile || not seen.(!p.var.id) do
        dowhile := false;
        p := Vec.last env.trail;
        assert (!p <> dummy_lit);
        confl := !p.var.reason;
        undo_one env
      done;
      decr counter
    done;
    Vec.set out_learnt 0 !p.neg;
    Logs.debug (fun k -> k"learn %a"
      Fmt.(iter ~sep:sp Vec.iter pp_lit) out_learnt);
    out_learnt, !bt_lvl

  (* let reduce_db env =
    let nb = nb_learnts env in
    let m = nb / 2 in
    let limit = env.cla_inc /. (Float.of_int nb) in
    let j = ref 0 in
    for i = 0 to nb - 1 do
      let learnt = Vec.get env.learnts i in
      if not @@ Clause.locked learnt
        && (i < m || (i >= m && learnt.cla_activity < limit)) then
        Clause.remove learnt
      else begin
        Vec.set env.learnts !j learnt;
        incr j
      end
    done;
    Vec.shrink env.learnts !j *)

  let simplify_db env =
    let[@inline always] aux clas =
      let j = ref 0 in
      for i = 0 to Vec.size clas - 1 do
        let cla = Vec.get clas i in
        if Clause.simplify cla then
          Clause.remove cla
        else begin
          Vec.set clas !j cla;
          incr j
        end
      done
    in
    if propagate env <> None then false
    else begin
      aux env.constrs;
      aux env.learnts;
      true
    end

  exception Sat of bool array
  exception Unsat

  let search env ~max_conflicts:_ ~max_learnts:_ ~var_decay ~cla_decay =
    assert (decision_level env = env.root_lvl);
    Logs.debug (fun k -> k"search");
    let conflict = ref 0 in
    env.var_decay <- 1. /. var_decay;
    env.cla_decay <- 1. /. cla_decay;
    while true do
      match propagate env with
      | Some confl ->
          Logs.debug (fun k -> k"conflict %a" pp_clause confl);
          incr conflict;
          if decision_level env = env.root_lvl then
            raise Unsat
          else begin
            let learnt, bt_lvl = analyze env confl in
            let bt_lvl = max bt_lvl env.root_lvl in
            Logs.debug (fun k -> k"backjump to the level %i" bt_lvl);
            cancel_until env bt_lvl;
            record env learnt;
            decay_activities env
          end
      | None ->
          (* No conflict have been found. *)
          (* if decision_level env = 0 then
            (* FIXME: removing asserts is dangereous here. *)
            assert (simplify_db env = true); *)
          (* if nb_learnts env - nb_assigns env >= max_learnts then
            reduce_db env; *)
          if nb_assigns env = nb_vars env then
            let model = Array.init (nb_vars env) (fun _ -> false) in
            for i = 0 to Array.length model - 1 do
              model.(i) <- equal_lbool (Vec.get env.vars i).value True
            done;
            cancel_until env env.root_lvl;
            raise (Sat model)
          (* else if !conflict >= max_conflicts then
            (* Reached bound on number of conflicts. Force a restart. *)
            cancel_until env env.root_lvl *)
          else
            let p = (O.select env.vars).lit in
            let res = assume env p in
            assert res
    done

  let solve env assumptions =
    let _ =
      match assumptions with
      | [] -> Logs.debug (fun k -> k"solve")
      | _ ->
          Logs.debug (fun k -> k"solve assumptions:%a"
            (Fmt.list pp_lit) assumptions)
    in
    let var_decay = ref 0.95 in
    let cla_decay = ref 0.999 in
    let max_conflicts = ref 100 in
    let max_learnts = ref ((nb_constrs env)/3) in
    try
      (* Push incremental assumptions. *)
      List.iter (fun lit ->
        if not @@ assume env lit || propagate env <> None then begin
          cancel_until env 0;
          raise Exit
        end
      ) assumptions;
      env.root_lvl <- decision_level env;
      while true do
        search env ~max_conflicts:!max_conflicts ~max_learnts:!max_learnts
          ~var_decay:!var_decay ~cla_decay:!cla_decay;
        max_conflicts := 
          (Float.of_int !max_conflicts) *. 1.5 |> Int.of_float;
        max_learnts := 
          (Float.of_int !max_learnts) *. 1.1 |> Int.of_float;
      done;
      assert false
    with
    | Exit | Unsat -> (Unsat : answer)
    | Sat model ->
        cancel_until env 0;
        Sat model
end
