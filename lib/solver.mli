type var [@@deriving show]
type clause [@@deriving show]
type lit [@@deriving show]

module Syntax : sig
  val neg : lit -> lit
  val lit : var -> lit
end

type lbool = True | False | Unknown [@@deriving show, compare, equal]
val value : var -> lbool

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

module Make (_ : Var_order with type env := var Vec.t) : S
