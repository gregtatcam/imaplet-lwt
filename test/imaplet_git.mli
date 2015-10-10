open Lwt

exception InvalidObject

type t

module Key :
sig
  type t 
  val create : string list -> t
  val of_unix : string -> t
  val to_string : t -> string
end

val create : repo:string -> t Lwt.t

val read_opt : t -> Key.t -> string option Lwt.t

val read_exn : t -> Key.t -> string Lwt.t
