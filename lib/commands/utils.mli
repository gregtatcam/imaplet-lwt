(*
 * Copyright (c) 2013-2014 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Sexplib
open Parsemail

val formated_capability : string -> string

val formated_id : string -> string

val to_plist : string -> string

val fl_to_str : Imaplet_types.mailboxFlags -> string

val str_to_fl : string -> Imaplet_types.mailboxFlags

val substr : string -> start:int -> size:(int option)-> string

val concat_path : string -> string -> string

val make_email_message : string -> Mailbox.Message.t

val option_value : 'a option -> default:'a -> 'a

val option_value_exn : ?ex:exn -> 'a option -> 'a

val list_find : 'a list -> ('a -> bool) -> bool

val list_findi : 'a list -> (int -> 'a -> bool) -> (int * 'a) option

val with_file : ?lock:bool -> string -> flags:Lwt_unix.open_flag list ->
  perms:Lwt_unix.file_perm -> mode:'a Lwt_io.mode -> 
  f:('a Lwt_io.channel -> 'b Lwt.t) -> 'b Lwt.t 

val exists : string -> ?alt:Lwt_unix.file_kind -> Lwt_unix.file_kind -> bool Lwt.t

val lines : string -> int

val list_of_str_sexp : string -> string list

val str_sexp_of_list : string list -> string

val lines_of_file : string -> init:'a -> f:(string -> 'a -> 'a Lwt.t) -> 'a Lwt.t

val with_timeout : float -> (unit -> 'a Lwt.t) -> (exn -> 'a Lwt.t) -> 'a Lwt.t

val get_interfaces : unit -> string list Lwt.t

val fold_email_with_file : string -> ('a -> string -> 'a Lwt.t) -> 'a -> 'a Lwt.t

val files_of_directory : string -> ('a -> string -> 'a Lwt.t) -> 'a -> [`Ok of 'a |`NoDir] Lwt.t 

(* replace %user% template (or regx) in path with the user, user@domain is
 * handled as domain/user *)
val user_path : ?regx:string -> path:string -> user:string -> unit -> string

(* user@domain -> user * domain *)
val parse_user : string -> string * (string option)

(* user@domain -> domain/user *)
val user_to_path : string -> string
