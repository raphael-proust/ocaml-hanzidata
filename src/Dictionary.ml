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

module Decoding : sig

   exception Error of string
   val entry_of_string: string -> entry

end = struct

   (* Error management helpers *)
   exception Error of string
   ;;
   let decoding_error_of_jsonm_error e =
      Jsonm.pp_error Format.str_formatter e;
      let e = Format.flush_str_formatter () in
      Error e
   ;;
   let handle_other_cases other_lexeme_message = function
      | `Lexeme _ -> raise (Error other_lexeme_message)
      | `End -> raise (Error "Unexpected early end")
      | `Error e -> raise (decoding_error_of_jsonm_error e)
      | `Await -> assert false (* the sources are never `Manual *)
   ;;
   let unopt name v = match v with
      | Some v -> v
      | None -> raise (Error ("Missing field " ^ name))
   ;;


   (* Decoders for individual fields *)
   let character jdecoder =
      match Jsonm.decode jdecoder with
      | `Lexeme (`String s) -> begin
         let udecoder = Uutf.decoder (`String s) in
         match Uutf.decode udecoder with
         | `Uchar u -> begin
            match Uutf.decode udecoder with
            | `End -> u
            | `Uchar _ -> raise (Error "Expected single uchar in character field")
            | `Malformed _ -> raise (Error "Malformed unicode string in character field")
            | `Await -> assert false
            end
         | `End -> raise (Error "Expected uchar in character field")
         | `Malformed _ -> raise (Error "Malformed unicode string in character field")
         | `Await -> assert false
         end
      | j -> handle_other_cases "Expected string value for character field" j
   ;;

   let definition jdecoder =
      match Jsonm.decode jdecoder with
         | `Lexeme (`String s) -> s
         | j -> handle_other_cases "Expected string in definition field" j
   ;;

   let pinyin jdecoder =
      match Jsonm.decode jdecoder with
      | `Lexeme `As ->
         let rec loop pys =
            match Jsonm.decode jdecoder with
            | `Lexeme (`String py) -> loop (py :: pys)
            | `Lexeme `Ae -> List.rev pys
            | j -> handle_other_cases "Unexpected value in pinyin array" j
         in
         loop []
      | j -> handle_other_cases "Expected array in pinyin field" j
   ;;

   let parse_decomposition s =
      let udecoder = Uutf.decoder (`String s) in
      let get_uchar () =
         match Uutf.decode udecoder with
         | `Uchar u -> u
         | `Malformed _ -> raise (Error "Malformed unicode string in decomposition field")
         | `End -> raise (Error "Unexpected end of decomposition string")
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
   ;;
   let decomposition jdecoder =
      match Jsonm.decode jdecoder with
      | `Lexeme (`String s) -> parse_decomposition s
      | j -> handle_other_cases "Expected string value in decomposition field" j
   ;;

   let radical jdecoder =
      match Jsonm.decode jdecoder with
      | `Lexeme (`String s) -> begin
         let udecoder = Uutf.decoder (`String s) in
         match Uutf.decode udecoder with
         | `Uchar u -> begin
            match Uutf.decode udecoder with
            | `End -> u
            | `Uchar _ -> raise (Error "Expected single uchar in radical field")
            | `Malformed _ -> raise (Error "Malformed unicode string in radical field")
            | `Await -> assert false
            end
         | `End -> raise (Error "Expected uchar in radical field")
         | `Malformed _ -> raise (Error "Malformed unicode string in radical field")
         | `Await -> assert false
         end
      | j -> handle_other_cases "Expected string value for radical field" j
   ;;

   let etymology jdecoder =
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
   ;;

   let matches jdecoder =
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
   ;;


   let entry_of_string s =

      (* Set up *)
      let character_ref = ref None in
      let definition_ref = ref None in
      let pinyin_ref = ref None in
      let decomposition_ref = ref None in
      let etymology_ref = ref None in
      let radical_ref = ref None in
      let matches_ref = ref None in
      (* Decoder for a whole entry *)
      let rec decoder jdecoder =
         match Jsonm.decode jdecoder with
         | `Lexeme (`Name "character") ->
            character_ref := Some (character jdecoder);
            decoder jdecoder
         | `Lexeme (`Name "definition") ->
            definition_ref := Some (definition jdecoder);
            decoder jdecoder
         | `Lexeme (`Name "pinyin") ->
            pinyin_ref := Some (pinyin jdecoder);
            decoder jdecoder
         | `Lexeme (`Name "decomposition") ->
            decomposition_ref := Some (decomposition jdecoder);
            decoder jdecoder
         | `Lexeme (`Name "etymology") ->
            etymology_ref := Some (etymology jdecoder);
            decoder jdecoder
         | `Lexeme (`Name "radical") ->
            radical_ref := Some (radical jdecoder);
            decoder jdecoder
         | `Lexeme (`Name "matches") ->
            matches_ref := Some (matches jdecoder);
            decoder jdecoder
         | `Lexeme `Oe -> begin
            match Jsonm.decode jdecoder with
            | `End -> ()
            | j -> handle_other_cases "Expected end of string after end of object" j
            end
         | j -> handle_other_cases "Unexpected lexeme in entry" j
      in
      let jdecoder = Jsonm.decoder (`String s) in

      (* Actual parsing *)
      begin match Jsonm.decode jdecoder with
         | `Lexeme `Os -> ()
         | j -> handle_other_cases "Expected object start at start of string" j
      end;
      decoder jdecoder;

      (* Post-process *)
      {
         character = unopt "character" !character_ref;
         definition = !definition_ref;
         pinyin = unopt "pinyin" !pinyin_ref;
         decomposition = unopt "decomposition" !decomposition_ref;
         etymology = !etymology_ref;
         radical = unopt "radical" !radical_ref;
         matches = unopt "matches" !matches_ref;
      }
   ;;

end


let read ?(path = "./makemeahanzi/dictionary.txt") () =
   let ic = open_in path in
   let rec line_loop decoded_lines =
      try begin
         let line = input_line ic in
         try
            let decoded_line = Decoding.entry_of_string line in
            line_loop (decoded_line :: decoded_lines)
         with
            | Decoding.Error e ->
               Printf.fprintf stderr "line: %s\nerror: %s\n%!" line e;
               line_loop decoded_lines
      end with
         | End_of_file -> List.rev decoded_lines

   in
   let entries = line_loop [] in
   close_in ic;
   entries
;;
