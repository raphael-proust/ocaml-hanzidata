
type entry = {
   character: Uchar.t;
   strokes: string list;
   medians: (int * int) list list;
}

module Decoding : sig

   exception Error of string
   val entry_of_string: string -> entry

end = struct

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

   let decode_character jdecoder =
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
   let decode_strokes jdecoder =
      match Jsonm.decode jdecoder with
      | `Lexeme `As ->
         let rec loop ss =
            match Jsonm.decode jdecoder with
            | `Lexeme (`String s) -> loop (s :: ss)
            | `Lexeme `Ae -> List.rev ss
            | j -> handle_other_cases "Unexpected value in pinyin array" j
         in
         loop []
      | j -> handle_other_cases "Expected array in pinyin field" j
   ;;
   let decode_coordinate jdecoder =
      match Jsonm.decode jdecoder with
      | `Lexeme (`Float fx) -> begin
         let x = int_of_float fx in
         match Jsonm.decode jdecoder with
         | `Lexeme (`Float fy) ->
            let y = int_of_float fy in
            (x,y)
         | j -> handle_other_cases "Expected a number in second component of coordinate" j
         end
      | j -> handle_other_cases "Expected a number in first component of coordinate" j
   ;;
   let decode_coordinate_list jdecoder =
      let rec loop coords =
         match Jsonm.decode jdecoder with
         | `Lexeme `As -> begin
            let coord = decode_coordinate jdecoder in
            match Jsonm.decode jdecoder with
            | `Lexeme `Ae -> loop (coord :: coords)
            | j -> handle_other_cases "Expected end of array after coordinates" j
            end
         | `Lexeme `Ae -> List.rev coords
         | j -> handle_other_cases "Expected coordinates in coordinate list" j
      in
      loop []
   ;;
   let decode_medians jdecoder =
      match Jsonm.decode jdecoder with
      | `Lexeme `As -> begin
         let rec loop meds =
            match Jsonm.decode jdecoder with
            | `Lexeme `As ->
               let med = decode_coordinate_list jdecoder in
               loop (med :: meds)
            | `Lexeme `Ae -> List.rev meds
            | j -> handle_other_cases "Expected array of coordinates in median" j
         in
         loop []
         end
      | j -> handle_other_cases "Expected array in medians" j
   ;;


   let entry_of_string s =

      (* Setup *)
      let jdecoder = Jsonm.decoder (`String s) in

      (* Decoder for a whole entry *)
      let entry =
         match Jsonm.decode jdecoder with
         | `Lexeme `Os -> begin
         match Jsonm.decode jdecoder with
         | `Lexeme (`Name "character") -> begin
            let character = decode_character jdecoder in
            match Jsonm.decode jdecoder with
            | `Lexeme (`Name "strokes") -> begin
               let strokes = decode_strokes jdecoder in
               match Jsonm.decode jdecoder with
               | `Lexeme (`Name "medians") ->
                  let medians = decode_medians jdecoder in

                  { character; strokes; medians; }

               | j -> handle_other_cases "Expected medians component" j
               end
            | j -> handle_other_cases "Expected strokes component" j
            end
         | j -> handle_other_cases "Expected character component" j
         end
         | j -> handle_other_cases "Expected object in entry" j
      in
      match Jsonm.decode jdecoder with
      | `Lexeme `Oe -> entry
      | j -> handle_other_cases "Expected end of object after components" j
   ;;


end


let read ?n ?(path = "./makemeahanzi/graphics.txt") () =
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
