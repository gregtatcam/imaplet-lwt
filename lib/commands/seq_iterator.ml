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

module SequenceIterator :
  sig
    type t

    (* sequence -> max in seq *)
    val create : sequence -> ?rev:bool -> ?test:bool -> int -> int -> t

    val single : sequence -> int option

    val next : t -> [`Ok of int|`End]

  end =
  struct
    (* sequence, rev, next element in sequence, current counter, max for the
       counter, overal min and max for the mailbox, delta(±1) *)
    type t = sequence * bool * int ref*int ref*int ref*int * int * int

    let create seq ?(rev=false) ?(test=false) min max = 
      if rev then
        (seq,rev,ref (List.length seq - 1), ref 0, ref 0, min,max,-1)
      else
        (seq,rev,ref 0, ref 0, ref 0, min,max,1)

    (* is this a single or a range *)
    let single seq =
      if List.length seq = 1 then
        match (List.hd seq) with
        | SeqNumber sn ->
          (
          match sn with
          | Number n -> Some n
          | Wild -> None
          )
        | SeqRange _ -> None
      else
        None

    (* get next in sequence *)
    let next t = 
      let s,rev,nc,c,cmax,min,max,dl = t in
      let get_n m = function
        | Number n -> n
        | Wild -> m
      in
      let update mi mx =
        c := mi;
        cmax := mx;
        `Ok !c
      in
      c := !c + dl;
      if rev && !c < !cmax || rev = false && !c > !cmax then (
        if rev && !nc < 0 || rev = false && !nc >= List.length s then
          `End
        else (
          let seq = List.nth s !nc in
          nc := !nc + dl; (* ref to the next element in the sequence *)
          match seq with
          | SeqNumber sn ->
            (match sn with
            | Number n -> update n n
            | Wild -> update max max
            )
          | SeqRange (sn1,sn2) ->
              if rev then
                update (get_n max sn2) (get_n min sn1)
              else
                update (get_n min sn1) (get_n max sn2)
        )
      ) else
        `Ok !c
  end
