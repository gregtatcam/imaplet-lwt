(*
 * Copyright (c) 2013-2016 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
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

module MapStr = Map.Make(String)

type t = {pool_mutex:Lwt_mutex.t;lock_pool: (int * Lwt_mutex.t) Map.Make(String).t ref}

let create pool_mutex lock_pool =
  {pool_mutex;lock_pool}

let get_acct_lock t name =
  Lwt_mutex.with_lock t.pool_mutex (fun() ->
    if MapStr.mem name !(t.lock_pool) then (
      let (refcnt,mutex) = MapStr.find name !(t.lock_pool) in
      t.lock_pool := MapStr.add name (refcnt+1,mutex) !(t.lock_pool);
      return mutex
    ) else (
      let mutex = Lwt_mutex.create () in
      t.lock_pool := MapStr.add name (1,mutex) !(t.lock_pool);
      return mutex
    )
  )

let rem_acct_lock t name =
  Lwt_mutex.with_lock t.pool_mutex (fun() ->
    if MapStr.mem name !(t.lock_pool) then (
      let (refcnt,mutex) = MapStr.find name !(t.lock_pool) in
      let refcnt = refcnt - 1 in
      if refcnt = 0 then
        t.lock_pool := MapStr.remove name !(t.lock_pool)
    );
    return ()
  )

let with_lock t name f =
  Lwt.finalize (fun() ->
    get_acct_lock t name >>= fun mutex ->
    Lwt_mutex.with_lock mutex f >>= fun res ->
    return res)
  (fun () -> rem_acct_lock t name)
