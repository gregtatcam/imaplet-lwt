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
open Lwt

type t

type result = [`Ok|`Error of string]

(* 
 * return `Ok to continue posting more messages or `Done to stop
 *)
type post = ((from:string -> rcpt:string -> (unit -> string option Lwt.t) -> result Lwt.t) ->
  result Lwt.t)

(* log * ip * port * ehlo * from * rcptto * data-feed *)
val create : 
  ?log:([`None|`Error|`Debug|`Info1|`Info2|`Info3] -> string -> unit) ->
  string -> int -> bool -> post -> t

val send_server :t -> result Lwt.t
