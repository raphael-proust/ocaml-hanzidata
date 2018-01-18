
module Parsing : sig

   exception Error of string
   val handle_other_cases: string ->
      [< `Await | `End | `Error of [< Jsonm.error ] | `Lexeme of [< Jsonm.lexeme ] ] ->
      'a

   type 'a t = Jsonm.decoder -> 'a
   val run: 'a t -> Jsonm.decoder -> 'a
   val return: 'a -> 'a t
   val bind: 'a t -> ('a -> 'b t) -> 'b t
   val (>>=): 'a t -> ('a -> 'b t) -> 'b t
   val map: 'a t -> ('a -> 'b) -> 'b t
   val (>|=): 'a t -> ('a -> 'b) -> 'b t
   val eat: Jsonm.lexeme -> string -> unit t
   val string: string -> string t
   val int: string -> int t
   val array:
      ([ `Lexeme of Jsonm.lexeme ] -> 'a t) ->
      [ `Lexeme of [ `As ] ] ->
      'a list t
   val kv: string -> (string -> 'k) -> ('k -> 'v t) -> ('k * 'v) t

end = struct

   exception Error of string
   let parsing_error_of_jsonm_error e =
      Jsonm.pp_error Format.str_formatter e;
      let e = Format.flush_str_formatter () in
      Error e
   ;;
   let handle_other_cases other_lexeme_message = function
      | `Lexeme (_: [< Jsonm.lexeme ]) -> raise (Error other_lexeme_message)
      | `End -> raise (Error "Unexpected early end")
      | `Error (e: [< Jsonm.error ]) -> raise (parsing_error_of_jsonm_error e)
      | `Await -> assert false (* the sources are never `Manual *)
   ;;

   type 'a t = Jsonm.decoder -> 'a;;
   let run m j = m j;;

   let return x = fun _ -> x;;

   let bind p f = fun jdecoder -> f (p jdecoder) jdecoder;;
   let (>>=) = bind;;

   let map p f = bind p (fun v -> return (f v))
   let (>|=) = map;;

   let string m jdecoder =
      match Jsonm.decode jdecoder with
         | `Lexeme (`String s) -> s
         | j -> handle_other_cases m j
   ;;

   let int m jdecoder =
      match Jsonm.decode jdecoder with
         | `Lexeme (`Float i) -> int_of_float i
         | j -> handle_other_cases m j
   ;;

   let eat  l e jdecoder =
      match Jsonm.decode jdecoder with
      | `Lexeme ll when l = ll -> ()
      | j -> handle_other_cases "Expected end of object after components" j
   ;;

   let array f las jdecoder =
      let rec loop xs =
         match Jsonm.decode jdecoder with
         | `Lexeme `Ae -> List.rev xs
         | `Lexeme _ as ll ->
            let x = f ll jdecoder in
            loop (x :: xs)
         | j -> handle_other_cases "Unexpected value in array" j
      in
      loop []
   ;;

   let kv m fk fv jdecoder =
      match Jsonm.decode jdecoder with
      | `Lexeme (`Name k) ->
         let k = fk k in
         let v = fv k jdecoder in
         (k, v)
      | j -> handle_other_cases m j
   ;;

end
