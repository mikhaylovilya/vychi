open Base

let f x = (2. **. -.x) +. (0.5 *. (x **. 2.)) -. 10.

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module type MONADARGS = sig
  include MONAD

  (* val fail : string -> 'a t *)
end

let _ =
  let args = Sys.get_argv () in
  match Array.length args with
  | 2 ->
    let arg = args.(1) in
    f @@ Float.of_string arg |> Float.to_string |> Caml.print_endline
  | _ -> "invalid args" |> Caml.print_endline
;;

let _ = Caml.print_endline "Hello from main.ml"
