(*
 * test irmin git/network vs file/network io
 * - create a thousand unique files (compressed to be comparable with irmin)
 * - dump the same files into irmin(git)
 * - fork a tcp client (**)
 * - launch two parallel threads
 * - create a stream
 * - in first thread 
 *  . read from a file/irmin
 *  . add to the stream
 * - in second thread
 *  . read from the stream
 *  . write to the client
 *  (**) repeat several time for file/irmin
 *)
open Lwt
open Irmin_unix

exception InvalidCommand

module Store = Irmin.Basic (Irmin_git.FS) (Irmin.Contents.String)

let compress = ref false

type config = {init:bool;compress:bool;yield:bool;content:string}

let create_content size = 
  Random.init max_int;
  String.init size (fun i -> char_of_int ((mod) (Random.int 1_000_000) 255)) 

(* command line *)
let rec args i _init _compr _size _yield =
  if i >= Array.length Sys.argv then
    _init,_compr,_size,_yield
  else
    match Sys.argv.(i) with 
    | "-i" -> args (i+1) true _compr _size _yield
    | "-c" -> args (i+1) _init true _size _yield
    | "-s" -> args (i+2) _init _compr (int_of_string Sys.argv.(i+1)) _yield
    | "-y" -> args (i+1) _init _compr _size true
    | _ -> raise InvalidCommand

let commands f =
  let init,compress,size,yield = args 1 false false 100_000 false in
  f {init;compress;content=create_content size;yield}
(* done command line *)

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

let unique_content buff i =
  let id = Printf.sprintf "%07d" i in
  String.blit id 0 buff 0 7;
  buff

let fold_i n f init =
  let rec _fold i acc =
    if i > n then
      return acc
    else (
      f i acc >>= fun acc ->
      _fold (i + 1) acc
    )
  in
  _fold 1 init

let root = "./tmp_ir_fs"
let irmin_root = Filename.concat root "irmin"
let files_root = Filename.concat root "files"

let init () =
  Lwt_unix.system (Printf.sprintf "rm -rf %s" root) >>= fun _ ->
  Lwt_unix.mkdir root 0o777>>= fun () ->
  Lwt_unix.mkdir files_root 0o777>>= fun () ->
  Lwt_unix.mkdir irmin_root 0o777

let create_files config =
  Printf.printf "creating files\n%!";
  fold_i 1_000 (fun i acc ->
    Printf.printf "file %d\r%!" i;
    let file = Printf.sprintf "./tmp_ir_fs/files/%d" i in
    Lwt_io.with_file ~flags:[Unix.O_WRONLY;Unix.O_CREAT;Unix.O_NONBLOCK] ~mode:Lwt_io.Output file (fun w ->
      let content = unique_content config.content i in
      Lwt_io.write w (if config.compress then do_compress ~header:true content else content))
  ) ()

let task msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let owner = "imaplet <imaplet@openmirage.org>" in
  Irmin.Task.create ~date ~owner msg

let create_store () =
  let _config = Irmin_git.config
    ~root:"./tmp_ir_fs/irmin"
    ~bare:true () in
  Store.create _config task

let create_irmin config =
  Printf.printf "creating irmin\n%!";
  create_store () >>= fun store ->
  fold_i 1_000 (fun i acc ->
    Printf.printf "irmin %d\r%!" i;
    Store.update (store (Printf.sprintf "updating %d" i)) 
      ["root";string_of_int i] (unique_content config.content i)
  ) ()

let addr = (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1",20001))

let read_response r =
  let rec _read () =
    Lwt_io.read r ~count:(2048*5) >>= fun buff ->
    if buff = "" then (
      return ()
    ) else (
      _read ()
    )
  in
  _read ()

let command cmd =
  Lwt_io.with_connection addr (fun (r,w) ->
    Printf.printf "connected to the server, requesting %s\n%!" cmd;
    Lwt_io.write_line w cmd >>= fun () ->
    read_response r
  )

let client () =
  Printf.printf "started client\n%!";
  let cmds =
    ["file";"file";"file";"file";"file";"irmin";"irmin";"irmin";"irmin";"irmin";"quit"]
  in
  Lwt_list.iter_s command cmds

let process_file config id =
  let file = Filename.concat files_root id in
  Lwt_io.with_file ~flags:[Unix.O_NONBLOCK] ~mode:Lwt_io.Input file (fun ic ->
    Lwt_io.read ic >>= fun buff ->
    return (if config.compress then do_uncompress ~header:true buff else buff)
  )

let process_irmin store config id =
  Store.read_exn (store (Printf.sprintf "reading %s" id)) ["root"; id]

let writet = ref 0.
let readt = ref 0.

let write_to_client ts strm w =
  let rec _write i =
    Lwt_stream.get strm >>= function
    | Some buff -> 
      Printf.printf "writing %d\r%!" i;
      let t = Unix.gettimeofday () in
      if i = 1 then
        Printf.printf "### first write delay %.04f\n%!" (t -. ts);
      Lwt_io.write w buff >>= fun () -> 
      writet := !writet +. (Unix.gettimeofday () -. t);
      _write (i+1)
    | None -> return ()
  in
  _write 1

let process config msg w store_reader =
  readt := 0.;
  writet := 0.;
  Printf.printf "processing %s\n%!" msg;
  let (strm,push_strm) = Lwt_stream.create () in
  let t = Unix.gettimeofday () in
  Lwt.join [
  begin
  fold_i 1_000 (fun i acc ->
    let t = Unix.gettimeofday () in
    store_reader config (string_of_int i) >>= fun buff ->
    readt := !readt +. (Unix.gettimeofday () -. t);
    push_strm (Some buff);
    (if config.yield then Lwt_unix.yield () else return ()) >>= fun () ->
    return acc
  ) () >>= fun () -> push_strm None; return ()
  end
  ;
  begin
  write_to_client (Unix.gettimeofday()) strm w
  end
  ] >>= fun () ->
  let total = Unix.gettimeofday () -. t in
  Printf.printf "%s total read %.04f, write %.04f, overall %.04f\n" msg !readt !writet total;
  return ()

let () =
commands (fun config ->
  Lwt_main.run (
    begin
    if config.init then (
      init () >>= fun () ->
      create_files config >>= fun () ->
      create_irmin config 
    ) else
      return ()
    end >>= fun () ->
    if Lwt_unix.fork () = 0 then (
      Lwt_unix.sleep 1. >>= fun () ->
      client ()
    ) else (
      let mutex = Lwt_mutex.create () in
      Lwt_mutex.lock mutex >>= fun () ->
      let server = Lwt_io.establish_server ~backlog:10 addr (fun (r,w) ->
        let loop () =
          Lwt_io.read_line_opt r >>= function
          | Some cmd ->
            Printf.printf "server processing command %s\n%!" cmd;
            begin
              match cmd with
              | "file" -> 
                process config "file" w process_file >>= fun () ->
                Lwt_io.close r >>= fun () -> Lwt_io.close w
              | "irmin" -> 
                create_store () >>= fun store ->
                process config "irmin" w (process_irmin store) >>= fun () ->
                Lwt_io.close r >>= fun () -> Lwt_io.close w
              | _ -> Lwt_io.close w >>= fun () -> Lwt_mutex.unlock mutex; return ()
            end
          | None -> Lwt_io.close w >>= fun () -> Lwt_mutex.unlock mutex; return ()
        in
        async(fun () -> loop ());
        Printf.printf "done iteration\n";
      ) in
      Printf.printf "established server\n%!";
      Lwt_mutex.lock mutex >>= fun () ->
      Lwt_io.shutdown_server server;
      return ()
    )
  )
)
