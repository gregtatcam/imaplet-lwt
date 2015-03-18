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
open Imaplet
open Commands

let get_messages path start stop =
  Lwt_io.with_file ~mode:Lwt_io.Input path (fun ic ->
    let buffer = Buffer.create 10000 in
    let rec loop ic buffer cnt = 
      let read ic cnt stop =
        if cnt > stop then
          return None
        else
          Lwt_io.read_line_opt ic 
      in
      read ic cnt stop >>= function
      | Some line ->
      let line = line ^ "\n" in
      let regx = "^from [^ ]+ " ^ Regex.dayofweek ^ " " ^ Regex.mon ^ " " ^
        "[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [0-9]+" in
      if Regex.match_regex line ~case:false ~regx && Buffer.length buffer > 0 then (
        if cnt >= start then
          Printf.printf "%s%!" (Buffer.contents buffer);
        Buffer.clear buffer;
        Buffer.add_string buffer line;
        loop ic buffer (cnt+1)
      ) else (
        Buffer.add_string buffer line;
        loop ic buffer cnt
      )
      | None -> return ()
    in
    loop ic buffer 1 
  )

let () =
  Lwt_main.run (
    let path = Sys.argv.(1) in
    let _ = Regex.match_regex Sys.argv.(2) ~regx:"\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?" in
    let start = int_of_string (Str.matched_group 1 Sys.argv.(2)) in
    let stop = 
      try
        let stop = Str.matched_group 3 Sys.argv.(2) in
        int_of_string stop
      with _ -> start
    in
    get_messages path start stop
  )
