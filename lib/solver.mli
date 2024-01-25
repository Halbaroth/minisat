type lbool = True | False | Unknown [@@deriving show, compare, equal]

module Var : sig 
  type t 
  (** Type of a propositional variable. *)

  val dummy : t
  (** Dummy value used by the vectors. *)

  val compare : t -> t -> int
  (** [compare v1 v2] compares the two variables [v1] and [v2]. *)

  val value : t -> lbool
  (** [value v] returns the current value of the variable [v] in the 
      internal trail of the solver. *)

  val pp : t Fmt.t
  (** [pp ppf v] prints a representation of [v] on the formatter [ppf]. *)
end

module Lit : sig 
  type t
  (** Type of a literal. *)

  val dummy : t
  (** Dummy value used by the vectors. *)

  val compare : t -> t -> int
  (** [compare l1 l2] compares the two literals [l1] and [l2]. *)

  val (-~) : t -> t
  (** [-~ l] is the negation of the literal [l]. *)

  val var : t -> Var.t 
  (** [var l] returns the underlying variable of [l]. *)

  val value : t -> lbool
  (** [value v] returns the current value of the literal [l] in the 
      internal trail of the solver. *)

  val pp : t Fmt.t
  (** [pp ppf l] prints a representation of [l] on the formatter [ppf]. *)
end

module type Var_order = sig
  type t 

  val make : unit -> t
  val new_var : t -> Var.t -> unit
  val update_var : t -> Var.t -> unit
  val update_all : t -> unit
  val undo : t -> Var.t -> unit
  val select : t -> Var.t
end

type answer = private
  | Sat of Lit.t array 
  | Unsat
  | Timeout
  (** The time limit is reached. *)

val pp_answer : answer Fmt.t

module type S = sig
  type t
  (** Type of the environment of the SAT solver. The variable type ['a]
      is a phantom type which prevent. *)

  val make : ?timeout:int -> unit -> t
  (** [make ?timeout ()] creates a new solver with a timeout. 
      Omitting the argument [timeout] means the solver has no limit time. 

      @raise invalid_arg if the timeout is negative. *)

  val of_dimacs_file : ?timeout:int -> string -> t
  (** [of_dimacs_file file] loads the content of the dimacs file [file].
      
      @raise invalid_arg if the timeout is negative. *)

  val new_var : t -> Lit.t
  (** [make_var env] creates a new propositional variable in the 
      environment [env]. 
      
      @return the positive literal associated with this variable. *)

  val add_clause : t -> Lit.t list -> bool
  (** [add_clause ] *)

  val simplify_db : t -> bool

  val check : t -> Lit.t list -> answer
end

module Make (_ : Var_order) : S
