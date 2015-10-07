let readt = ref 0.
let writet = ref 0.
let readmetat = ref 0.
let fetcht = ref 0.

let init () =
  readt := 0.;
  writet := 0.;
  readmetat := 0.;
  fetcht := 0.;
;;

let get_readt () = !readt
let get_writet () = !writet
let get_readmetat () = !readmetat
let get_fetcht () = !fetcht
let add_readt t = 
  readt := !readt +. t
let add_readmetat t = 
  readmetat := !readmetat +. t
let add_writet t = 
  writet := !writet +. t
let add_fetcht t = 
  fetcht := !fetcht +. t
