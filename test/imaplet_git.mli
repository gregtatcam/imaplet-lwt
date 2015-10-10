open Lwt

exception InvalidObject

type t

val create : repo:string -> t Lwt.t

val read_opt : t -> key -> string option Lwt.t

val read_exn : t -> key -> string Lwt.t
