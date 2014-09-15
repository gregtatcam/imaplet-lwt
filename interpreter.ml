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
open Core.Std
open Imaplet_types
open Regex

exception InvalidSequence

  
(** get seq_number from the string **)
let get_seq_number_exn (number:string) : (seq_number) =
  if number = "*" then
    Wild
  else if match_regex number ~regx:"^[1-9][0-9]*$" then
    Number (int_of_string number)
  else
    raise InvalidSequence

(** get set_set structure from seq-number [":" seq-number]? **)
let get_seq_set_exn (str_set:string) : (seq_set) =
  let num_list = Str.split (Str.regexp ":") str_set in
  let len = List.length num_list in
  if ( len > 0 && len <= 2) = false then
    raise InvalidSequence
  else if len = 1 then
    let n = get_seq_number_exn (List.nth_exn num_list 0) in
    SeqNumber (n)
  else
    let n1 = get_seq_number_exn (List.nth_exn num_list 0) in
    let n2 = get_seq_number_exn (List.nth_exn num_list 1) in
    SeqRange (n1,n2)


(** parse sequence set into occaml structure for execution 
 seq-number = nz-number|"*"
 seq-range = seq-number ":" seq-number
 sequence-set = (seq-number | seq-range) ["," sequence-set]*
 **)
let get_sequence (sequence:string) : ( seq_set list) =
  let lofset = Str.split (Str.regexp ",") sequence in
  List.fold lofset ~init:[] ~f:(fun acc range ->
    let r = get_seq_set_exn range in r :: acc)

