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

type client_context = {
  id: Int64.t;
  user: string;
  outch: Lwt_io.output_channel;
  capability: string list ref;
  mailbox: string;
}

type context = {
  id : Int64.t;
  connections : (client_context list) ref;
  commands : Imaplet_types.clientRequest Stack.t ref;
  netr : Lwt_io.input_channel ref;
  netw : Lwt_io.output_channel ref;
  state : Imaplet_types.state ref;
  mailbox : Amailbox.t ref;
  starttls : unit -> (Lwt_io.input_channel * Lwt_io.output_channel) Lwt.t;
  highestmodseq : [`None|`Sessionstart of int64|`Highestmodseq] ref;
  capability: string list ref;
  config: Server_config.imapConfig;
}
