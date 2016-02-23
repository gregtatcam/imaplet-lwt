open Lwt

module MapStr = Map.Make(String)
module MapInt = Map.Make(Int32)

exception InvalidCommand

let parse_dataset str =
  Re.split (Re_posix.compile_pat ",") str

let parse_labels str =
  Re.split (Re_posix.compile_pat ",") str

let rec args i archive dataset labels folder size =
  if i >= Array.length Sys.argv then
    archive,dataset,labels,folder,size
  else
    match Sys.argv.(i) with 
    | "-archive" -> args (i+2) Sys.argv.(i+1) dataset labels folder size
    | "-dataset" -> args (i+2) archive (parse_dataset Sys.argv.(i+1)) labels folder size
    | "-labels" -> args (i+2) archive dataset (parse_labels Sys.argv.(i+1)) folder size
    | "-size" -> args (i+2) archive dataset labels folder 
      ((int_of_string Sys.argv.(i+1)) * 1024 * 1024)
    | "-folder" -> args (i+2) archive dataset labels Sys.argv.(i+1) size
    | _ -> raise InvalidCommand

let usage () =
  Printf.fprintf stderr "usage: get_messages_rand -archive filename -dataset
  [n1,n2..] -labels [label1,...,labeln] -folder [folder] -size [maxmsgsizeinMB]\n%!"

let commands f =
  try 
    let archive,dataset,labels,folder,size = 
      args 1 "" [] [] "X-Gmail-Labels" max_int in
    if archive = "" then
      raise InvalidCommand
    else
      try 
        f archive dataset labels folder size
      with ex -> Printf.printf "%s\n%!" (Printexc.to_string ex)
  with _ -> usage ()

let re_postmark = Re_posix.compile_pat ~opts:[`ICase] 
  "^(from ((\"[^\"]+\")|([^ \"]+)) (mon|tue|wed|thu|fri|sat|sun) (jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec))"

let is_postmark str =
  Re.execp re_postmark str

let re_folder_ folder = 
  let folder = Printf.sprintf "^%s:[ ]*([^\r\n]+)" folder in
  Re_posix.compile_pat ~opts:[`ICase] folder

let re_messageid = 
  Re_posix.compile_pat ~opts:[`ICase] "message-id:[ ]*([^ \r\n]+)"

let is_messageid str =
  Re.execp re_messageid str

let is_folder re_folder str =
  Re.execp re_folder str

let get_message ic buffer re_folder f init =
  let rec loop folder messageid acc =
    Lwt_io.read_line_opt ic >>= function
    | None -> 
      if Buffer.length buffer > 0 then (
        let content = Buffer.contents buffer in
        Buffer.clear buffer;
        f acc folder messageid content >>= function
        | `Ok acc -> return acc
        | `Done acc -> return acc
      ) else
        return acc
    | Some line ->
      let line = line ^ "\n" in
      if is_postmark line && (Buffer.length buffer >0) then (
        let content = Buffer.contents buffer in
        Buffer.clear buffer;
        Buffer.add_string buffer line;
        f acc folder messageid content >>= function
        | `Ok acc -> loop "" "" acc 
        | `Done acc -> return acc
      ) else (
        let len = String.length line in
        let messageid =
          if messageid = "" && is_messageid line then (
            let subs = Re.exec re_messageid line in
            Re.get subs 1
          ) else
            messageid
        in
        let folder = 
          if folder = "" && is_folder re_folder line then (
            let subs = Re.exec re_folder line in
            Re.get subs 1
          ) else
            folder
        in
        Buffer.add_string buffer line;
        loop folder messageid acc
      )
  in
  loop "" "" init
  
let fold_email_with_file file re_folder f init =
  Lwt_io.with_file ~mode:Lwt_io.Input file (fun ic ->
    get_message ic (Buffer.create 100) re_folder f init
  )

let () =
  commands (fun archive dataset labels folder size ->
    Random.self_init ();
    Lwt_main.run (
      fold_email_with_file archive (re_folder_ folder) 
      (fun (cnt,unique,all_messages) folder messageid message ->
        (* include unique messages, exclude messages matching the folder,
         * exclude messages exceeding size *)
        if MapStr.mem messageid unique || String.length message > size ||
            Re.execp (Re_posix.compile_pat ~opts:[`ICase] "calendar$") folder then (
          Printf.printf "out %d\r%!" cnt;
          return (`Ok (cnt+1,unique,all_messages))
        ) else (
          Printf.printf "in  %d\r%!" cnt;
          return (`Ok (cnt+1,MapStr.add messageid "" unique, message :: all_messages))
        )
      ) (0,MapStr.empty,[]) >>= fun (_,_,all_messages) ->
      let length = List.length all_messages in
      Printf.printf "selected messages %d\n%!" length;
      (* loop over requested dataset sizes *)
      Lwt_list.iter_s (fun ds ->
        let outfile = Printf.sprintf "%s.mbox" ds in
        let ds = int_of_string ds in
        Lwt_io.with_file ~mode:Lwt_io.Output outfile (fun oc ->
          let rec random exists =
            if MapInt.cardinal exists = ds then
              return ()
            else (
              let n = Int32.of_int (Random.int length) in
              if MapInt.mem n exists then
               random exists
              else (
                Printf.printf "%d: %d\r%!" ds (MapInt.cardinal exists + 1);
               let message = List.nth all_messages (Int32.to_int n) in
               Lwt_io.write oc message >>= fun () ->
               random (MapInt.add n 0 exists)
              )
            )
          in
          random MapInt.empty
        )
      ) dataset
    )
  )
