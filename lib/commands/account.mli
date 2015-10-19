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

type acct_config = {
  acct_data_store : [`Irmin|`Workdir|`Mailbox|`Maildir|`Gitl]; (* type of storage, irmin/maildir/workdir supported *)
  acct_encrypt : bool; (* encrypt messages, default true *)
  acct_compress : bool; (* compress messages, but not attachments, default true *)
  acct_compress_attach : bool; (* compress attachments, default false *)
  acct_compress_repo : bool; (* compress repo, default false *)
  acct_auth_required: bool; (* require user authentication, priv key encrypted with password, default true *)
  acct_maildir_parse: bool; (* parse message into MIME parts when in maildir storage format, default true *)
  acct_single_store: bool; (* single-store attachments in irmin and workdir format, default true *)
  acct_hybrid: bool; (* hybrid of irmin and workdir store (store should be set to irmin, default false *)
}

val authenticate : Imaplet_types.authtype -> string ->
  [`Ok of Imaplet_types.response * string * string * acct_config option | `Error of Imaplet_types.response] Lwt.t

val login : string -> string ->
  [`Ok of Imaplet_types.response * string * string * acct_config option | `Error of Imaplet_types.response] Lwt.t

val authenticate_user : ?b64:bool -> ?users:string -> string -> ?password:string
-> unit -> (string*string option*bool*acct_config option) Lwt.t

val plain_auth : string -> (string*string option*bool*acct_config option) Lwt.t
