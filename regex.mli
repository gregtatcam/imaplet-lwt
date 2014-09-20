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
val match_regex_i: ?case:bool -> regx:string -> string -> int

val match_regex: ?case:bool -> regx:string -> string -> bool

val replace: regx:string -> tmpl:string -> string -> string

val date_regex : string

val date_dqregex : string

val date_time_dqregex : string

val email_date_regex : string

val quote : string -> string

val dequote : string -> string

val list_of : string -> string

val dlist_of : string -> string

val astring : string

val group : string -> string

val dot : string

val sol : string

val eol : string

val optional : string -> string

val orxl : string list -> string

val all_of_it : string -> string

val nz_number : string

val number : string

val space : string

val bkt_list_of : string -> string

val ang_list_of : string -> string

val crlf : string

val append_regex : string

val lappend_regex : string
