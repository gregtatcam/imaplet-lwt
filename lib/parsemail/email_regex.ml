open Core_replace

type t = Re.re
type regex = t

module Template = struct
  type t = regex * string
end

module type Creators_intf = sig
  val create : string -> t
  val create_m : string -> t

  module Template : sig
    type t = Template.t
    type regex
    val create : regex:regex -> template:string -> t
  end with type regex := t
end

module type Accessors_intf = sig
  module Infix : sig
    val (=~) : string -> t -> bool
  end

  module Match : sig
    type t
    val by_index : t -> int -> string
    val by_name : t -> string -> int -> string
  end

  module Template : sig
    type t
    val apply : t -> string -> string;;
  end with type t := Template.t

  val matches : t -> string -> bool;;
  val apply : t -> string -> Match.t option
  val split_at : t -> string -> string list;;
end

module Accessors : Accessors_intf = struct
  let matches t str = Re.execp t str;;
  let apply t str = try Some (Re.exec t str) with Not_found -> None
  let split_at t str = Re.split t str

  module Infix = struct
    let (=~) str t = Re.execp t str;;
  end

  module Match = struct
    type t = Re.substrings
    let by_index m i = Re.get m i
    let by_name m n i = Re.get m i
  end

  module Template = struct
    let apply (regex, template) str =
      (Re.replace_string regex ~by:template str)
    ;;
  end
end

module Creators = struct
  let create = Re_posix.compile_pat ~opts:[`ICase];;
  let create_m =
    Re_posix.compile_pat ~opts:[
      `ICase;
      `Newline;
    ]
  ;;

  module Template = struct
    type t = Template.t;;

    let create ~regex ~(template : string) : t =
      (* how to check for validity (in re2 valid_rewrite_template ) *)
        (regex, template)
  end
end


