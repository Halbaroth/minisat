open Ppx_compare_lib.Builtin

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
  mutable value : Core.lbool;
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

module Var = struct 
  let dummy = dummy_var 
  let compare = compare_var 
  let[@inline always] activity { var_activity; _ } = var_activity
  let[@inline always] value v = v.value

  let pp ppf ({ id; dimacs_id; _ } as var) =
    if var == dummy_var then
      Fmt.pf ppf "(dummy_var)"
    else
      let id =
        match dimacs_id with
        | Some id -> id
        | None -> id
      in
      Fmt.int ppf id
end

module Lit = struct 
  let dummy = dummy_lit 
  let compare = compare_lit
  let[@inline always] (-~) lit = lit.neg
  let[@inline always] var lit = lit.var

  let[@inline always] value { var; sign; _ } =
    if sign then Core.neg_lbool var.value else var.value

  let pp ppf ({ var; sign; _ } as lit) =
    if lit == dummy_lit then
      Fmt.pf ppf "(dummy_lit)"
    else
      if sign then Var.pp ppf var
      else Fmt.pf ppf "-%a" Var.pp var
end

module Clause = struct 
  let dummy = dummy_clause

  let calc_reason { lits; _ } p =
    assert (p == dummy_lit || p == lits.(0));
    let j = if p == dummy_lit then 0 else 1 in
    let size = Array.length lits in
    let reason = Vec.make ~dummy:Lit.dummy (size-j) in
    for i = j to size - 1 do
      assert (Lit.value lits.(i) == False);
      Vec.push reason lits.(i).neg
    done;
    reason

  (* Check if a learnt clause is locked, that is the clause is the reason
     of some propagation. *)
  let locked ({lits; learnt; _ } as clause) =
    assert learnt;
    compare_clause lits.(0).var.reason clause = 0

  (* Remove the clause from the watch lists. *)
  let remove ({lits; _} as clause) =
    Vec.remove lits.(0).neg.watched clause;
    Vec.remove lits.(1).neg.watched clause

  let pp ppf ({ lits; _ } as clause) =
    if clause == dummy then
      Fmt.pf ppf "(dummy_clause)"
    else
      Fmt.(array ~sep:(const string "∨") Lit.pp |> box) ppf lits

  (* Simplify a clause by removing all its false literals. This
     function can be called only at top-level with an empty propagation
     queue.

     @return [true] if and only if the clause is already true (and can 
             be completely removed as we are at the top-level). *)
  let simplify clause =
    Logs.debug (fun k -> k"simplify the clause %a" pp clause);
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
      clause.lits <- Array.init !j (fun i -> lits.(i));
      Logs.debug (fun k -> k"now the clause is %a" pp clause);
      false
    with Exit -> true

  let undo _ _ = ()
end 

module type Var_order = sig
  type t 

  val make : unit -> t
  val new_var : t -> var -> unit
  val update_var : t -> var -> unit
  val update_all : t -> unit
  val undo : t -> var -> unit
  val select : t -> var 
end

module type S = sig
  type t 
  type order

  val make : unit -> t
  val new_var : t -> lit
  val add_clause : t -> lit list -> bool
  val simplify_db : t -> bool
  val check : t -> lit list -> lit Core.answer
end

module Make (O : Var_order) = struct
  type order = O.t 

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
    order : O.t;

    (* Propagation queue. *)
    propagations: lit Queue.t;

    (* Assignments. *)
    vars: var Vec.t;
    trail: lit Vec.t;
    trail_lim: int Vec.t;
  }

  exception Unsat
  exception Sat of lit array
  exception Conflict of clause

  let[@inline always] nb_vars env = Vec.size env.vars
  let[@inline always] nb_assigns env = Vec.size env.trail
  let[@inline always] nb_constrs env = Vec.size env.constrs
  let[@inline always] nb_learnts env = Vec.size env.learnts
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
    O.update_var env.order var

  let var_decay_activity env =
    env.var_inc <- env.var_inc *. env.var_decay

  let cla_rescale_activity env =
    (* TODO: Check if we have to rescale the activity of constraints also? *)
    Vec.iter 
      (fun cla -> cla.cla_activity <- cla.cla_activity *. 1e-100)
      env.learnts;
    env.cla_inc <- env.cla_inc *. 1e-100

  let cla_bump_activity env cla =
    cla.cla_activity <- cla.cla_activity +. env.cla_inc;
    if cla.cla_activity > 1e100 then
      var_rescale_activity env
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
    let lits = Vec.to_array ps in 
    Array.fast_sort Lit.compare lits;
    let res = Vec.make ~dummy:Lit.dummy 17 in 
    let prev = ref Lit.dummy in
    Array.iter (fun lit ->
      if Lit.compare !prev lit <> 0 then begin 
        prev := lit;
        if Core.compare_lbool (Lit.value lit) False <> 0 then 
          Vec.push res lit
      end
    ) lits;
    res

  let enqueue env ?(from = Clause.dummy) p =
    match Lit.value p with
    | False -> false
    | True -> true
    | Unknown ->
        Logs.debug (fun k -> k"enqueue literal %a with reason %a" Lit.pp 
          p Clause.pp from);
        (* Store the new fact. *)
        let () =
          if p.sign then
            p.var.value <- False
          else
            p.var.value <- True
        in
        p.var.dlvl <- decision_level env;
        p.var.reason <- from;
        Vec.push env.trail p;
        Queue.push p env.propagations;
        true

  let propagate_in_clause env clause p =
    Logs.debug (fun k -> k"propagate literal %a in clause %a" Lit.pp p
      Clause.pp clause);
    let lits = clause.lits in
    assert (Array.length lits >= 2);
    (* Make sure the false literal is lits.(1). *)
    if Lit.compare lits.(0) p.neg = 0 then begin
      lits.(0) <- lits.(1);
      lits.(1) <- p.neg
    end;
    (* If the first literal is true, the clause is already satisfied. *)
    if Core.equal_lbool (Lit.value lits.(0)) True then begin
      Vec.push p.watched clause;
      assert (Vec.size p.watched > 0);
      true
    end
    else
      (* Look for a new literal to watch. *)
      try
        for i = 2 to Array.length lits - 1 do
          if not @@ Core.equal_lbool (Lit.value lits.(i)) False then begin
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
      if Vec.exists (fun lit -> Core.equal_lbool (Lit.value lit) True) ps then
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
          Fmt.(iter ~sep:sp Vec.iter Lit.pp) ps);
        res
    | res, None ->
        res

  let record env lits =
    Logs.debug (fun k -> k"record the clause %a" 
        Fmt.(iter ~sep:(const string "∨") Vec.iter Lit.pp) lits);
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
    Vec.push env.trail_lim lim;
    Logs.debug (fun k -> k"assume %a at level %i" Lit.pp p
      (decision_level env));
    enqueue env p

  (* Unbind the last variable on the trail. *)
  let undo_one env =
    let p = Vec.last env.trail in
    p.var.value <- Unknown;
    p.var.reason <- dummy_clause;
    p.var.dlvl <- -1;
    O.undo env.order p.var;
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
    Vec.push env.vars var;
    O.new_var env.order var;
    Logs.debug (fun k -> k "new var %a" Var.pp var);
    lit

  let make () = {
    root_lvl = 0;
    constrs = Vec.make ~dummy:dummy_clause 5;
    learnts = Vec.make ~dummy:dummy_clause 5;
    cla_inc = 0.5;
    cla_decay = -0.3;
    var_inc = 0.5;
    var_decay = -0.3;
    order = O.make ();
    propagations = Queue.create ();
    vars = Vec.make ~dummy:dummy_var 5;
    trail = Vec.make ~dummy:dummy_lit 5;
    trail_lim = Vec.make ~dummy:(-1) 5;
  }

  let add_cnf env Dimacs.Loc.{ data = (_ , clauses); _ } =
    let open Dimacs in
    let cache : (int, lit) Hashtbl.t = Hashtbl.create 17 in
    List.iter (fun Loc.{ data = clause; _ } ->
      let clause =
        List.fold_left (fun acc Loc.{ data = l; _ } ->
          let lit =
            match Hashtbl.find cache (Int.abs l) with
            | lit -> lit 
            | exception Not_found ->
              let lit = new_var ~dimacs_id:(Some (Int.abs l)) env in
              Hashtbl.add cache (Int.abs l) lit;
              lit
          in
          assert (l <> 0);
          let lit = if l > 0 then lit else Lit.((-~) lit) in
          lit :: acc
        ) [] clause
        |> List.rev
      in
      let _ : bool = add_clause env clause in ()
    ) clauses

  let new_var env = new_var ~dimacs_id:None env

  let propagate env =
    while Queue.length env.propagations > 0 do
      let p = Queue.pop env.propagations in
      Logs.debug (fun k -> k"propagate literal %a" Lit.pp p);
      (* TODO: replace the copy by a move as we don't use the 
         old watches vector anymore. *)
      let tmp = Vec.copy p.watched in
      (* FIXME: is it necessary? *)
      Vec.clear p.watched;
      for i = 0 to Vec.size tmp - 1 do
        let clause = Vec.get tmp i in
        if not @@ propagate_in_clause env clause p then
          begin
            (* We found a conflict. *)
            for j = i + 1 to Vec.size tmp - 1 do
              Vec.push p.watched (Vec.get tmp j)
            done;
            Queue.clear env.propagations;
            raise_notrace (Conflict clause)
          end
      done
    done

  let analyze env confl =
    assert (decision_level env > env.root_lvl);
    Logs.debug (fun k -> k"analyze %a" Clause.pp confl);
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
      Fmt.(iter ~sep:(const string "∨") Vec.iter Lit.pp |> box) out_learnt);
    out_learnt, !bt_lvl

  let reduce_db env =
    let nb = nb_learnts env in
    let lim = env.cla_inc /. (Float.of_int nb) in
    let i = ref 0 in
    let j = ref 0 in
    while !i < (nb / 2) do 
      let learnt = Vec.get env.learnts !i in
      if not @@ Clause.locked learnt then 
        Clause.remove learnt 
      else begin 
        Vec.set env.learnts !j learnt; 
        incr j
      end; 
      incr i
    done;
    while !i < nb do 
      let learnt = Vec.get env.learnts !i in 
      if not @@ Clause.locked learnt && learnt.cla_activity < lim then 
        Clause.remove learnt
      else begin 
        Vec.set env.learnts !j learnt; 
        incr j
      end; 
      incr i
    done;
    Vec.shrink env.learnts (!i - !j)

  let simplify_db env =
    Logs.debug (fun k -> k"simplify the clause database");
    let[@inline always] aux clauses =
      let j = ref 0 in
      let sz = Vec.size clauses in
      for i = 0 to sz - 1 do
        let c = Vec.get clauses i in
        if Clause.simplify c then
          Clause.remove c
        else begin
          Vec.set clauses !j c;
          incr j
        end
      done;
      Vec.shrink clauses (sz - !j)
    in
    match propagate env with 
    | exception (Conflict _) -> false
    | () ->
      aux env.constrs;
      aux env.learnts;
      true

  exception Restart

  let search env ~max_conflicts ~max_learnts ~var_decay ~cla_decay =
    assert (decision_level env = env.root_lvl);
    Logs.debug (fun k -> k"search");
    let conflict = ref 0 in
    env.var_decay <- 1. /. var_decay;
    env.cla_decay <- 1. /. cla_decay;
    while true do
      match propagate env with
      | exception (Conflict confl) ->
          Logs.debug (fun k -> k"conflict %a" Clause.pp confl);
          incr conflict;
          if decision_level env = env.root_lvl then
            raise_notrace Unsat
          else begin
            let learnt, bt_lvl = analyze env confl in
            let bt_lvl = max bt_lvl env.root_lvl in
            Logs.debug (fun k -> k"backjump to the level %i" bt_lvl);
            cancel_until env bt_lvl;
            record env learnt;
            decay_activities env
          end
      | () ->
          (* No conflict have been found. *)
          if decision_level env = 0 then begin
            let res = simplify_db env in
            (* We cannot reach a contradiction here as we exclude this 
               case in the above pattern matching. *)
            assert res; 
          end;
          if nb_learnts env - nb_assigns env >= max_learnts then
            reduce_db env;
          if nb_assigns env = nb_vars env then
            let model = Vec.to_array env.trail in
            cancel_until env env.root_lvl;
            raise_notrace (Sat model)
          else if !conflict >= max_conflicts then begin
            (* Reached bound on number of conflicts. Force a restart. *)
            cancel_until env env.root_lvl;
            raise_notrace Restart
          end
          else
            let p = (O.select env.order).lit in
            let res = assume env p in
            assert res
    done

  let check env assumptions =
    let () =
      match assumptions with
      | [] -> 
        Logs.debug (fun k -> k"solve")
      | _ ->
        Logs.debug (fun k -> k"solve with assumptions %a"
          Fmt.(list ~sep:sp Lit.pp |> box) assumptions)
    in
    let var_decay = ref 0.95 in
    let cla_decay = ref 0.999 in
    let max_conflicts = ref 100 in
    let max_learnts = ref ((nb_constrs env)/3) in
    try
      (* Push incremental assumptions. *)
      List.iter (fun lit ->
        if not @@ assume env lit then 
          match propagate env with 
          | () -> ()
          | exception (Conflict _) ->
            cancel_until env 0;
            raise_notrace Exit
      ) assumptions;
      env.root_lvl <- decision_level env;
      while true do
        try
          search env ~max_conflicts:!max_conflicts ~max_learnts:!max_learnts
            ~var_decay:!var_decay ~cla_decay:!cla_decay;
        with Restart ->
          max_conflicts := 
            (Float.of_int !max_conflicts) *. 1.5 |> Int.of_float;
          max_learnts := 
            (Float.of_int !max_learnts) *. 1.1 |> Int.of_float;
          ()
      done;
      assert false
    with
    | Exit | Unsat -> (Unsat : _ Core.answer)
    | Sat mdl ->
      cancel_until env 0;
      Sat mdl
end
