type 'w var
(** Type of a propositional variable of the solver ['w]. *)

type 'w lit
(** Type of a literal of the solver ['w]. *)

module Var : sig 
  val dummy : 'w var
  (** Dummy value used by vectors. *)

  val activity : 'w var -> float

  val compare : 'w var -> 'w var -> int
  (** [compare v1 v2] compares the two variables [v1] and [v2]. *)

  val value : 'w var -> Core.lbool
  (** [value v] returns the current value of the variable [v] in the 
      internal trail of the solver. *)

  val pp : 'w var Fmt.t
  (** [pp ppf v] prints a representation of [v] on the formatter [ppf]. *)
end

module Lit : sig 
  val dummy : 'w lit
  (** Dummy value used by vectors. *)

  val compare : 'w lit -> 'w lit -> int
  (** [compare l1 l2] compares the two literals [l1] and [l2]. *)

  val (-~) : 'w lit -> 'w lit
  (** [-~ l] is the negation of the literal [l]. *)

  val var : 'w lit -> 'w var
  (** [var l] returns the underlying variable of [l]. *)

  val value : 'w lit -> Core.lbool
  (** [value v] returns the current value of the literal [l] in the 
      internal trail of the solver. *)

  val pp : 'w lit Fmt.t
  (** [pp ppf l] prints a representation of [l] on the formatter [ppf]. *)
end

module type Var_order = sig
  type 'w t 

  val make : unit -> 'w t
  (** [make ()] creates a new environment for the order heuristic.  
      This function is called during the creation of the solver ['w]. *)

  val new_var : 'w t -> 'w var -> unit
  (** Hook calling when the solver ['w] creates a new variable. *)

  val update_var : 'w t -> 'w var -> unit
  val update_all : 'w t -> unit
  val undo : 'w t -> 'w var -> unit

  val select : 'w t -> 'w var
  (** [select env] selects the next variable to decide. *)
end

(** Type safe API of the SAT solver. *)
module Solver : sig
  module type S = sig
    type wit
    (** Identifier of the SAT solver. *)

    val new_var : unit -> wit lit
    (** [make_var ()] creates a new propositional variable of the solver [wit]. 
        
        @return the positive literal associated with this variable. *)

    val add_clause : wit lit list -> bool
    (** [add_clause lits] adds the clause [lits] to the solver [wit]. *)

    val add_cnf : Dimacs.Ast.t -> unit

    val simplify_db : unit -> bool

    val check : wit lit list -> wit lit Core.answer
    (** [check lits] checks if there exists a model satisfying both the 
        clauses of the solver [wit] and the literals of [lits]. *)
  end

  type order = (module Var_order)

  type t = (module S)
  (** Type of the solver. *)

  val make : order:order -> unit -> t
  (** [make ~order ()] creates a new solver using the order heuristic 
      [order]. *)

  val of_dimacs_file : order:order -> string -> t
  (** [of_dimacs_file file] creates a new solver and loads the content of 
      the dimacs file [file]. *)
end

module Core : module type of Core 
module Vec : module type of Vec 
