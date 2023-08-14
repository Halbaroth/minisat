type 'a t

val make : dummy:'a -> int -> 'a t
val of_array : dummy:'a -> 'a array -> 'a t
val of_list : dummy:'a -> 'a list -> 'a t
val shrink : 'a t -> int -> unit
val pop : 'a t -> unit
val grow_to : 'a t -> ?pad:'a -> int -> unit
val clear : 'a t -> unit
val push : 'a t -> 'a -> unit
val last : 'a t -> 'a
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val mem : 'a -> 'a t -> bool
val find_first : p:('a -> bool) -> 'a t -> 'a
val remove : 'a t -> 'a -> unit
val size : 'a t -> int
val capacity : 'a t -> int
val to_array : 'a t -> 'a array
val to_list : 'a t -> 'a list
val copy : 'a t -> 'a t
val iter : f:('a -> unit) -> 'a t -> unit
val iteri : f:(int -> 'a -> unit) -> 'a t -> unit
val fold : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b
val exists : f:('a -> bool) -> 'a t -> bool
val for_all : f:('a -> bool) -> 'a t -> bool
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val pp : ?sep:string -> 'a Fmt.t -> 'a t Fmt.t
val show : ?sep:string -> 'a Fmt.t -> 'a t -> string
