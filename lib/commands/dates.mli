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

exception InvalidDate

module ImapDate :
  sig
    type t [@@deriving sexp]

    val of_string : string -> t
    val compare : t -> t -> int
    val of_tm : Unix.tm -> t
    val to_string : t -> string
    val t_of_sexp : Sexp.t -> t
    val sexp_of_t : t -> Sexp.t 
  end

module ImapTime : 
  sig
    type t [@@deriving sexp]

    val now : unit -> t
    val of_string : string -> t
    val of_float : float -> t
    val to_float : t -> float
    val to_date : t -> ImapDate.t
    val epoch : t
    val t_of_sexp : Sexp.t -> t
    val sexp_of_t : t -> Sexp.t 
    val to_string : t -> string
  end

val imapd_to_date_exn : string -> ImapDate.t

val imapd_to_date_time_exn : string -> ImapTime.t

val date_time_to_email : ImapTime.t -> string

val email_to_date_time_exn : string -> ImapTime.t

val day_of_week : int -> string

val int_to_month : int -> string

val postmark_date_time : ?time:ImapTime.t -> unit -> string
