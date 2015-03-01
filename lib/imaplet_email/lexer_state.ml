open Core_replace;;

module State = struct
  type t = [
    (** Initial state, parsing headers *)
    `Header  |

    (** Parsing the body of the message. The details are in the body state. *)
    `Content |

    (** The message should end here. If it doesn't, it's an error *)
    `Expected_eof
  ]
  ;;

  let initial = `Header;;
end

module Content = struct
  type t =
    Multipart of string list |
    Octet_stream

  let default = Octet_stream;;
end

type t =
  {
    mutable state : State.t;
    buf : Grammar.token Queue.t;
  }
;;

let create () = {
  state = State.initial;
  buf = Queue.create ();
};;

module Result = struct
  type t = {
    new_state : State.t option;
    tokens : Grammar.token list;
  }

  module Std = struct
    let return ?new_state tokens =
      {
        new_state = new_state;
        tokens = tokens
      }
    ;;

    let return_eof = return ~new_state:`Expected_eof [Grammar.EOF];;
    let return_error str = return [Grammar.ERROR str];;
  end
end

let combine t result =
  begin
    match result.Result.new_state with
    | Some state -> t.state <- state
    | None       -> ()
  end
  ;
  List.iter (fun tok -> Queue.push tok t.buf) 
    (result.Result.tokens)
;;

include Result.Std;;

module Error = struct
  let unexpected_char c = Printf.sprintf "Unexpected char: %c" c
  let unexpected_eof = "Unexpected end of file"
end
