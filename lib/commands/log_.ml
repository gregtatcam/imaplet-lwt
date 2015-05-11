(*
 * Copyright (c) 2013-2015 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
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
open Server_config

let oc = ref stderr

let set_log file =
  oc :=
  try
    if srv_config.log = "stderr" then
      stderr
    else if srv_config.log = "stdout" then
      stdout
    else if srv_config.log <> "" then
      open_out_gen [Open_append;Open_creat] 0o666 (Filename.concat srv_config.log file)
    else
      stderr
  with _ -> stderr

let is l allowed =
  List.exists (fun l1 -> l1 = l) allowed

(* `Error|`Info1|`Info2|`Info3|`Debug *)
let log level msg =
  let prt =
  match srv_config.log_level with
  | `Debug -> true
  | `Info3 when (is level [`Info3;`Info2;`Info1;`Error]) -> true 
  | `Info2 when (is level [`Info2;`Info1;`Error]) -> true 
  | `Info1 when (is level [`Info1;`Error]) -> true 
  | `Error when (is level [`Error]) -> true 
  | _ -> false
  in
  if prt then
    Printf.fprintf !oc "%s%!" msg
  else
    ()
