(** Types and helper functions for the data in dictionary.txt *)

type decomposition =
   | Component of Uchar.t
   | Unknown
   | LeftRight of decomposition * decomposition
   | AboveBelow of decomposition * decomposition
   | LeftMiddleRight of decomposition * decomposition * decomposition
   | AboveMiddleBelow of decomposition * decomposition * decomposition
   | FullSurround of decomposition * decomposition
   | AboveSurround of decomposition * decomposition
   | BelowSurround of decomposition * decomposition
   | LeftSurround of decomposition * decomposition
   | UpperLeftSurround of decomposition * decomposition
   | UpperRightSurround of decomposition * decomposition
   | LowerLeftSurround of decomposition * decomposition
   | Overlaid of decomposition * decomposition

type entry = {
   character: Uchar.t;
   definition: String.t option;
   pinyin: String.t list;
   decomposition: decomposition;
   etymology: (string * string) list option;
   radical: Uchar.t;
   matches: int list option list;
}

exception Decoding_error of string

let decode_line line =
   let character = ref None in
   let definition = ref None in
   let pinyin = ref None in
   let decomposition = ref None in
   let etymology = ref None in
   let radical = ref None in
   let matches = ref None in

   let decoding_error_of_jsonm_error e =
      Jsonm.pp_error Format.str_formatter e;
      let e = Format.flush_str_formatter () in
      Decoding_error e
   in
   let handle_other_cases other_lexeme_message = function
      | `Lexeme _ -> raise (Decoding_error other_lexeme_message)
      | `End -> raise (Decoding_error "Unexpected early end")
      | `Error e -> raise (decoding_error_of_jsonm_error e)
      | `Await -> assert false
   in

   let jdecoder = Jsonm.decoder (`String line) in

   let parse_decomposition s =
      let udecoder = Uutf.decoder (`String s) in
      let get_uchar () =
         match Uutf.decode udecoder with
         | `Uchar u -> u
         | `Malformed _ -> raise (Decoding_error "Malformed unicode string in decomposition field")
         | `End -> raise (Decoding_error "Unexpected end of decomposition string")
         | `Await -> assert false
      in
      let rec loop () =
         let u = get_uchar () in
         match Uchar.to_int u with
         | 0xFF1F -> Unknown
         | 0x2FF0 ->
            let l = loop () in
            let r = loop () in
            LeftRight (l, r)
         | 0x2FF1 ->
            let a = loop () in
            let b = loop () in
            AboveBelow (a, b)
         | 0x2FF2 ->
            let l = loop () in
            let m = loop () in
            let r = loop () in
            LeftMiddleRight (l, m, r)
         | 0x2FF3 ->
            let a = loop () in
            let m = loop () in
            let b = loop () in
            AboveMiddleBelow (a, m, b)
         | 0x2FF4 ->
            let o = loop () in
            let i = loop () in
            FullSurround (o, i)
         | 0x2FF5 ->
            let o = loop () in
            let i = loop () in
            AboveSurround (o, i)
         | 0x2FF6 ->
            let o = loop () in
            let i = loop () in
            BelowSurround (o, i)
         | 0x2FF7 ->
            let o = loop () in
            let i = loop () in
            LeftSurround (o, i)
         | 0x2FF8 ->
            let o = loop () in
            let i = loop () in
            UpperLeftSurround (o, i)
         | 0x2FF9 ->
            let o = loop () in
            let i = loop () in
            UpperRightSurround (o, i)
         | 0x2FFA ->
            let o = loop () in
            let i = loop () in
            LowerLeftSurround (o, i)
         | 0x2FFB ->
            let x = loop () in
            let y = loop () in
            Overlaid (x, y)
         | _ -> Component u
      in
      loop ()
   in
   let decode_decomposition () =
      match Jsonm.decode jdecoder with
      | `Lexeme (`String s) -> parse_decomposition s
      | j -> handle_other_cases "Expected string value in decomposition field" j
   in

   let decode_etymology () =
      match Jsonm.decode jdecoder with
      | `Lexeme `Os ->
         let rec loop kv =
            match Jsonm.decode jdecoder with
            | `Lexeme `Oe -> kv
            | `Lexeme (`Name k) -> begin
               match Jsonm.decode jdecoder with
               | `Lexeme (`String v) -> loop ((k, v) :: kv)
               | j -> handle_other_cases "Expected string value in etymology entry" j
               end
            | j -> handle_other_cases "Expected a dictionary in etymology field" j
         in
         loop []
      | j -> handle_other_cases "Exepcted string value in decomposition field" j
   in

   let decode_matches () =
      match Jsonm.decode jdecoder with
      | `Lexeme `As ->
         let decode_match () =
            let rec loop ns =
               match Jsonm.decode jdecoder with
               | `Lexeme (`Float f) ->
                  let n = int_of_float f in
                  loop (n :: ns)
               | `Lexeme `Ae -> List.rev ns
               | j -> handle_other_cases "Expected numbers in matches array array" j
            in
            loop []
         in
         let rec loop ms =
            match Jsonm.decode jdecoder with
            | `Lexeme `As ->
               let m = decode_match () in
               loop (Some m :: ms)
            | `Lexeme `Null ->
               loop (None :: ms)
            | `Lexeme `Ae -> List.rev ms
            | j -> handle_other_cases "Expected arrays in matches array" j
         in
         loop []
      | j -> handle_other_cases "Expected array in matches field" j
   in
   begin match Jsonm.decode jdecoder with
      | `Lexeme `Os -> ()
      | j -> handle_other_cases "Expected object start at start of line" j
   end;
   let rec decode_line () =
      match Jsonm.decode jdecoder with

      | `Lexeme (`Name "character") -> begin
         match Jsonm.decode jdecoder with
         | `Lexeme (`String s) -> begin
            let udecoder = Uutf.decoder (`String s) in
            match Uutf.decode udecoder with
            | `Uchar u -> begin
               match Uutf.decode udecoder with
               | `End -> character := Some u
               | `Await -> assert false
               | `Malformed _ -> raise (Decoding_error "Malformed unicode string in character field")
               | `Uchar _ -> raise (Decoding_error "Expected single uchar in character field")
               end
            | _ -> raise (Decoding_error "Expected uchar in character field")
            end
         | j -> handle_other_cases "Expected string value for character field" j
         end;
         decode_line ()

      | `Lexeme (`Name "definition") -> begin
         match Jsonm.decode jdecoder with
         | `Lexeme (`String s) -> definition := Some s
         | j -> handle_other_cases "Expected string in definition field" j
         end;
         decode_line ()

      | `Lexeme (`Name "pinyin") -> begin
         match Jsonm.decode jdecoder with
         | `Lexeme `As ->
            let rec loop pys =
               match Jsonm.decode jdecoder with
               | `Lexeme (`String py) -> loop (py :: pys)
               | `Lexeme `Ae -> List.rev pys
               | j -> handle_other_cases "Unexpected value in pinyin array" j
            in
            pinyin := Some (loop [])
         | j -> handle_other_cases "Expected array in pinyin field" j
         end;
         decode_line ()

      | `Lexeme (`Name "decomposition") ->
         decomposition := Some (decode_decomposition ());
         decode_line ()

      | `Lexeme (`Name "radical") -> begin
         match Jsonm.decode jdecoder with
         | `Lexeme (`String s) -> begin
            let udecoder = Uutf.decoder (`String s) in
            match Uutf.decode udecoder with
            | `Uchar u -> begin
               match Uutf.decode udecoder with
               | `End -> radical := Some u
               | _ -> failwith "Expected single uchar in radical field"
               end
            | _ -> failwith "Expected uchar in radical field"
            end
         | j -> handle_other_cases "Expected string value for radical field" j
         end;
         decode_line ()

      | `Lexeme (`Name "matches") ->
         matches := Some (decode_matches ());
         decode_line ()

      | `Lexeme (`Name "etymology") ->
         etymology := Some (decode_etymology ());
         decode_line ()

      | `Lexeme `Oe -> begin
         match Jsonm.decode jdecoder with
         | `End -> ()
         | j -> handle_other_cases "Expected end of line after end of object" j
         end

      | j -> handle_other_cases "Unexpected lexeme in entry" j

   in
   decode_line ();

   let unopt name v = match v with
      | Some v -> v
      | None -> raise (Decoding_error ("Missing field " ^ name))
   in

   {
      character = unopt "character" !character;
      definition = !definition;
      pinyin = unopt "pinyin" !pinyin;
      decomposition = unopt "decomposition" !decomposition;
      etymology = !etymology;
      radical = unopt "radical" !radical;
      matches = unopt "matches" !matches;
   }
;;

let read ?(path = "./makemeahanzi/dictionary.txt") () =
   let ic = open_in path in
   let rec line_loop decoded_lines =
      try begin
         let line = input_line ic in
         try
            let decoded_line = decode_line line in
            line_loop (decoded_line :: decoded_lines)
         with
            | Decoding_error e ->
               Printf.fprintf stderr "line: %s\nerror: %s\n%!" line e;
               line_loop decoded_lines
      end with
         | End_of_file -> decoded_lines

   in
   let lines_rev = line_loop [] in
   close_in ic;
   List.rev lines_rev
;;
