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
open Lwt
open Imaplet_email
open Storage_meta

module MapStr = Map.Make(String)

module type LazyEmail_intf =
  sig
    type c
    type t
    val empty : t
    val create : c -> t
    val header : ?incl:[`Regx of string|`Map of unit Map.Make(String).t] -> ?excl:unit Map.Make(String).t -> 
      t -> (string * string) list 
    val header_to_str : ?incl:[`Regx of string|`Map of unit Map.Make(String).t] -> 
      ?excl:unit Map.Make(String).t -> t -> string
    val content : t -> [`Data of string|`Message of t|`Multipart of t list] Lwt.t
    val raw_content : t -> string Lwt.t
    val to_string : ?incl:[`Regx of string|`Map of unit Map.Make(String).t] -> ?excl:unit Map.Make(String).t -> 
      t -> string Lwt.t
    val size : t -> int
    val lines : t -> int
  end

module type LazyEmail_inst =
  sig
    module LazyEmail : LazyEmail_intf
    val this : LazyEmail.t
  end

module type LazyMessage_intf =
  sig
    type c
    type t
    (* create instance of the lazy message. functions define how
     * parts of the message are retrieved by the storage implementation
     *)
    val create : c -> t
    val get_postmark : t -> string Lwt.t
    val get_headers_block : t -> string Lwt.t
    val get_content_block : t -> string Lwt.t
    val get_email : t ->  (module LazyEmail_inst) Lwt.t
    val get_message_metadata : t -> mailbox_message_metadata Lwt.t
    
  end

module type LazyMessage_inst =
  sig
    module LazyMessage : LazyMessage_intf
    val this : LazyMessage.t
  end

let build_lazy_message_inst
(type lm)
(module LM : LazyMessage_intf with type c = lm) 
(arg:lm)
=
  (module struct
    module LazyMessage = LM
    let this = LM.create arg
  end: LazyMessage_inst)

let build_lazy_email_inst
(type lm)
(module LE : LazyEmail_intf with type c = lm) 
(arg:lm)
=
  (module struct
    module LazyEmail = LE
    let this = LE.create arg
  end: LazyEmail_inst)

let lazy_email_of_t (type i) (module LE:LazyEmail_intf with type t = i) (inst:i) =
  (module struct 
    module LazyEmail = LE
    let this = inst
  end:LazyEmail_inst)
