(*
 * Copyright (c) 2013-2015 Gregory Tsipenyuk <gt303@cam.ac.uk>
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
open Imaplet
open Commands
open Server_config

type stun_record = {privateaddr:string; pubaddr:string; master:string; msgid:
  string; ports: int list}

(* 
 * cache stun record for the given domain from the received message
 *)
val cache_stun_records : string option -> string -> string

(* 
 * start quering stun server 
 *)
val start : imapConfig -> unit

(*
 * add stun header to the content/master
 *)
val add_header : content:string -> master:string -> string

(*
 * find stun record for the given domain
 * and Received: header
 *)
val match_stun_records : content:string -> domain:string -> stun_record option

(* 
 * convert list of ports to comma sep string 
 *)
val ports_to_string : int list -> string
