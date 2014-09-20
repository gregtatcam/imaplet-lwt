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
open Imaplet_types

module type SequenceIterator_intf :
  sig
    type t

    (* sequence -> max in seq *)
    val create : sequence -> int -> int -> t

    val single : sequence -> bool

    val next : t -> [`Ok of int|`End]

  end =
  struct
    (* sequence, next element in sequence, current counter, max for the
       counter, overal min and max for the mailbox *)
    type t = sequence * int ref*int ref*int ref*int * int

    let create seq min max = (seq,ref 0, ref 0, ref 0, min,max)

    (* is this a single or a range *)
    let single seq =
      let open Core.Std in
      if List.length seq = 1 then
        match (List.hd_exn seq) with
        | SeqNumber sn ->
          (
          match sn with
          | Number n -> true
          | Wild -> false
          )
        | SeqRange _ -> false
      else
        false

    (* get next in sequence *)
    let next t = 
      let open Core.Std in
      let s,nc,c,cmax,min,max = t in
      let get_n m = function
        | Number n -> n
        | Wild -> m
      in
      let update mi mx =
        c := mi;
        cmax := mx;
        `Ok !c
      in
      c := !c + 1;
      if !c > !cmax then (
        let seq = List.nth s !nc in
        match seq with
        | None -> `End
        | Some seq ->
          (
          nc := !nc + 1; (* ref to the next element in the sequence *)
          match seq with
          | SeqNumber sn ->
            (match sn with
            | Number n -> update n n
            | Wild -> update min max
            )
          | SeqRange (sn1,sn2) ->
              update (get_n min sn1) (get_n max sn2)
          )
      ) else
        `Ok !c
  end
