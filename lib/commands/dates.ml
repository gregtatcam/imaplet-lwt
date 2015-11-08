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
open Regex
open Unix
open Sexplib

exception InvalidDate

(** figure out seconds to convert to utc **)
let utc_sec zone =
  if match_regex zone ~regx:"^\\([+-]\\)\\([0-9][0-9]\\):?\\([0-9][0-9]\\)$" then (
    let sign = Str.matched_group 1 zone in
    let hours = int_of_string (Str.matched_group 2 zone) in
    let mins = int_of_string (Str.matched_group 3 zone) in
    if hours > 11 || mins > 40 then
      0
    else (
      let secs = (hours * 3600 + mins * 60) in
      if sign = "+" then
        secs * (-1)
      else
        secs
    )
  ) else
    0

let get_date str = 
  (int_of_string (Str.matched_group 1 str)) - 1900,
  (int_of_string (Str.matched_group 2 str)) - 1,
  int_of_string (Str.matched_group 3 str)

let tdiff = 
  let t = Unix.time() in
  let (tl,_) = Unix.mktime (Unix.localtime t) in
  let (tg,_) = Unix.mktime (Unix.gmtime t) in
  tl -. tg

module ImapDate :
  sig
    type t

    val of_string : string -> t
    val compare : t -> t -> int
    val of_tm : Unix.tm -> t
    val to_string : t -> string
    val t_of_sexp : Sexp.t -> t
    val sexp_of_t : t -> Sexp.t 
  end = struct
    type t = float

    let of_tm tm = 
      let (t,_) = Unix.mktime tm in
      t +. tdiff

    (* 2014-10-25 *)
    let of_string str = 
      if match_regex str
          ~regx:"^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)$"
          = false then
        raise InvalidDate
      else (
        let tm_year,tm_mon,tm_mday = get_date str in
        let tm = {Unix.tm_sec=0;tm_min=0;tm_hour=0;tm_mday;tm_mon;tm_year;tm_wday=0;
        tm_yday=0;tm_isdst=false} in
        of_tm tm
      )

    let compare d1 d2 = 
      Pervasives.compare d1 d2

    let to_string t =
      let tm = Unix.gmtime t in
      Printf.sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday

    let t_of_sexp sexp = Pervasives.float_of_string (Sexp.to_string sexp)

    let sexp_of_t t = Sexp.of_string (Pervasives.string_of_float t)

  end

module ImapTime :
  sig
    type t

    val now : unit -> t
    val of_string : string -> t
    val of_float : float -> t
    val to_float : t -> float
    val to_date : t -> ImapDate.t
    val epoch : t
    val t_of_sexp : Sexp.t -> t
    val sexp_of_t : t -> Sexp.t 
    val to_string : t -> string
  end =
  struct
    type t = float

    let epoch = 0.

    let now () = Unix.time()

    (* hh:mm:ss.ss *)
    let get_time str = 
      let l = Str.split (Str.regexp ":") str in
      (int_of_string (List.nth l 0),
       int_of_string (List.nth l 1),
       int_of_string (List.nth l 2))

    (* 2014-10-25 09:25:47.045025+01:00 *)
    let of_string str = 
      if match_regex str ~regx:(all_of_it sys_date_time_regex) = false then
        raise InvalidDate
      else (
        let tm_year,tm_mon,tm_mday = get_date str in
        let time = Str.matched_group 4 str in
        let frac_sec = Pervasives.float_of_string (try Str.matched_group 5 str with _ -> ".0") in 
        let zone_sec = utc_sec (try Str.matched_group 6 str with _ -> "+00:00") in
        let (tm_hour,tm_min,tm_sec) = get_time time in
        let tm = {Unix.tm_sec=tm_sec+zone_sec;tm_min;tm_hour;tm_mday;tm_mon;tm_year;tm_wday=0;
                  tm_yday=0;tm_isdst=false} in
        let (t,_) = Unix.mktime tm in
        let t = t +. frac_sec in
        (t +. tdiff)
      )

    let of_float f = f

    let to_float t = t

    (* 2014-10-25 *)
    let to_date t =
      ImapDate.of_tm (Unix.gmtime t)

    let t_of_sexp sexp = Pervasives.float_of_string (Sexp.to_string sexp)

    let sexp_of_t t = Sexp.of_string (Pervasives.string_of_float t)

    let to_string t =
      let tm = Unix.gmtime t in
      Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
        tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

  end 

let month_to_int = function 
  | "Jan" -> 0 
  | "Feb" -> 1
  | "Mar" -> 2
  | "Apr" -> 3
  | "May" -> 4
  | "Jun" -> 5
  | "Jul" -> 6
  | "Aug" -> 7
  | "Sep" -> 8
  | "Oct" -> 9
  | "Nov" -> 10
  | "Dec" -> 11
  | _ -> 0

let int_to_month = function 
  | 0 -> "Jan"
  | 1 -> "Feb"
  | 2 -> "Mar"
  | 3 -> "Apr"
  | 4 -> "May"
  | 5 -> "Jun"
  | 6 -> "Jul"
  | 7 -> "Aug"
  | 8 -> "Sep"
  | 9 -> "Oct"
  | 10 -> "Nov"
  | 11 -> "Dec"
  | _ -> "Jan"

let day_of_week = function
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wed"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | 6 -> "Sat"
  | _ -> "Sun"

let of_day_of_week = function
  | "Sun" -> 0
  | "Mon" -> 1
  | "Tue" -> 2
  | "Wed" -> 3
  | "Thu" -> 4
  | "Fri" -> 5
  | "Sat" -> 6
  | _ -> 0

let month_to_int12 m = (month_to_int m) + 1

(** convert date from IMAP string to OCaml Date.t 
 * dd-mmm-yyyy
 **)
let imapd_to_date_exn (date:string) : (ImapDate.t) =
  let d =
  (if match_regex date ~regx:date_dqregex then
    replace "\"" "" date
  else
    date
  ) in
  if match_regex d date_regex = false then (
    raise InvalidDate
  ) else (
    let parts = Str.split (Str.regexp "-") date in
    let cvtd = Printf.sprintf "%s-%02d-%02d" (List.nth parts 2) (month_to_int12 (List.nth parts 1)) (int_of_string (List.nth parts 0)) in
    let date = ImapDate.of_string cvtd in
    date
  )

(** convert date from append to Date: format **)
let imapd_to_date_time_exn date =
  if match_regex date ~regx:date_time_dqregex = false then
    raise InvalidDate
  else (
    try
    let date = replace "\"" "" date in
    let parts = Str.split (Str.regexp " ") date in
    let date = List.nth parts 0 in
    let time = List.nth parts 1 in
    let zone = List.nth parts 2 in
    let dparts = Str.split (Str.regexp "-") date in
    let day = List.nth dparts 0 in
    let month = List.nth dparts 1 in
    let year = List.nth dparts 2 in
    let str = Printf.sprintf "%s-%02d-%s %s" year (month_to_int12 month) day time in
    let tm = ImapTime.of_string str in
    let f = (ImapTime.to_float tm) +. (Pervasives.float_of_int (utc_sec zone)) in
    ImapTime.of_float f
    with e -> raise e
  )

let date_time_to_email (dt:ImapTime.t) : (string) =
  let tm = Unix.gmtime (ImapTime.to_float dt) in
    (Printf.sprintf "%s, %d %s %d %02d:%02d:%02d +0000" 
    (day_of_week tm.tm_wday) tm.tm_mday (int_to_month tm.tm_mon) (tm.tm_year + 1900) 
    tm.tm_hour tm.tm_min tm.tm_sec)

(** convert date/time in email format to Time.t
email:
[ day-of-week "," ] date FWS time [CFWS],
day month year
time-of-day FWS zone
hour ":" minute [ ":" second ]
Date: Mon, 7 Feb 1994 21:52:25 -0800 (PST) 
date/time: 2014-03-05 21:36:02.233417Z
**)
let email_to_date_time_exn date =
  let date = replace ~regx:"\"" ~tmpl:"" date in
  let date = replace ~regx:"^[ ]*\\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\), " 
    ~tmpl:"" date in
  if match_regex date ~regx:email_date_regex = false then
    raise InvalidDate;
  
  let parts = Str.split (Str.regexp " ") date in
  let day = int_of_string (List.nth parts 0) in
  let month = month_to_int12 (List.nth parts 1) in
  let year = List.nth parts 2 in
  let time = List.nth parts 3 in
  let time = 
  if match_regex time ~regx:"^[0-9][0-9]:[0-9][0-9]$" then
    List.nth parts 3 ^ ":00"
  else
    List.nth parts 3
  in
  let zone = List.nth parts 4 in
  let str = Printf.sprintf "%s-%02d-%02d %s" year month day time in
  let tm = ImapTime.of_string str in
  let f = (ImapTime.to_float tm) +. (Pervasives.float_of_int (utc_sec zone)) in
  ImapTime.of_float f

let postmark_date_time ?time () =
  let time = match time with
  | Some time -> ImapTime.to_float time
  | None -> ImapTime.to_float (ImapTime.now()) 
  in
  let tm = Unix.gmtime time in
  Printf.sprintf "%s %s %02d %02d:%02d:%02d %d"
    (day_of_week tm.Unix.tm_wday) (int_to_month tm.Unix.tm_mon) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec (1900+tm.Unix.tm_year)
