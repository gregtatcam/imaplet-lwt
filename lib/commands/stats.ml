let readt = ref 0.
let writet = ref 0.

let init () =
  readt := 0.;
  writet := 0.
;;

let get_readt () = !readt
let get_writet () = !writet
let add_readt t = readt := !readt +. t
let add_writet t = writet := !writet +. t
