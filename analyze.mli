(** Representation of data allocated on the OCaml heap. *)
type data =
| Int of int
| Ptr of int
| Atm of int (* tag *)
| Fun of int (* address *)

type obj =
| Struct of int * data array (* tag × data *)
| Int64 of Int64.t (* Primitive integer *)
| Float64 of float (* Primitive float *)
| String of string

module LargeArray :
sig
  type 'a t
  val empty : 'a t
  val length : 'a t -> int
  val make : int -> 'a -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
end
(** A data structure similar to arrays but allowing to overcome the 2^22 length
    limitation on 32-bit architecture. *)

val parse_channel : in_channel -> (data * obj LargeArray.t)
val parse_string : string -> (data * obj LargeArray.t)
