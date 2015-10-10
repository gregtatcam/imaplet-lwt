(* compression *)
let refill input =
  let n = String.length input in
  let toread = ref n in
  fun buf ->
    let m = min !toread (String.length buf) in
    String.blit input (n - !toread) buf 0 m;
    toread := !toread - m;
    m

let flush output buf len =
  Buffer.add_substring output buf 0 len

let flush output buf len =
  Buffer.add_substring output buf 0 len

let do_compress ?(header=false) input =
  let output = Buffer.create (String.length input) in
  Zlib.compress ~level:6 ~header (refill input) (flush output);
  Buffer.contents output

let do_uncompress ?(header=false) input =
  let output = Buffer.create (String.length input) in
  Zlib.uncompress ~header (refill input) (flush output);
  Buffer.contents output
(* done compression *)

