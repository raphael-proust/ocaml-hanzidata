
type entry = {
   character: Uchar.t;
   strokes: string list;
   medians: (int * int) list list;
}

module Parsing : sig

   exception Error of string
   val entry_of_string: string -> entry

end = struct

   exception Error of string
   ;;
   let parsing_error_of_jsonm_error e =
      Jsonm.pp_error Format.str_formatter e;
      let e = Format.flush_str_formatter () in
      Error e
   ;;
   let handle_other_cases other_lexeme_message = function
      | `Lexeme _ -> raise (Error other_lexeme_message)
      | `End -> raise (Error "Unexpected early end")
      | `Error e -> raise (parsing_error_of_jsonm_error e)
      | `Await -> assert false (* the sources are never `Manual *)
   ;;

   let parse_character jdecoder =
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
   let parse_strokes jdecoder =
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
   let parse_coordinate =
      let open Helpers.Parsing in
      int "Expected a number as first coordinate" >>= fun x ->
      int "Expected a number as second coordinate" >>= fun y ->
      return (x,y)
   ;;
   let parse_coordinate_list jdecoder =
      let rec loop coords =
         match Jsonm.decode jdecoder with
         | `Lexeme `As -> begin
            let coord = parse_coordinate jdecoder in
            match Jsonm.decode jdecoder with
            | `Lexeme `Ae -> loop (coord :: coords)
            | j -> handle_other_cases "Expected end of array after coordinates" j
            end
         | `Lexeme `Ae -> List.rev coords
         | j -> handle_other_cases "Expected coordinates in coordinate list" j
      in
      loop []
   ;;
   let parse_medians jdecoder =
      match Jsonm.decode jdecoder with
      | `Lexeme `As as las ->
         Helpers.Parsing.array
            (function
            | `Lexeme `As -> parse_coordinate_list
            | j -> handle_other_cases "Expected array of coordinates in median" j
            )
            las
            jdecoder
      | j -> handle_other_cases "Expected array in medians" j
   ;;


   let entry_of_string s =

      (* Setup *)
      let jdecoder = Jsonm.decoder (`String s) in

      (* Decoder for a whole entry *)
      let open Helpers.Parsing in
      run (
         eat `Os "Expected object in entry" >>= fun () ->
         eat (`Name "character") "Expected character component" >>= fun () ->
         parse_character >>= fun character ->
         eat (`Name "strokes") "Expected strokes component" >>= fun () ->
         parse_strokes >>= fun strokes ->
         eat (`Name "medians") "Expected medians component" >>= fun () ->
         parse_medians >>= fun medians ->
         eat `Oe "Expected end of object after components" >>= fun () ->
         return { character; strokes; medians; }
      ) jdecoder
   ;;


end


let read ?n ?(path = "./makemeahanzi/graphics.txt") () =
   let ic = open_in path in
   let rec line_loop parsed_lines =
      try begin
         let line = input_line ic in
         try
            let parsed_line = Parsing.entry_of_string line in
            line_loop (parsed_line :: parsed_lines)
         with
            | Parsing.Error e ->
               Printf.fprintf stderr "line: %s\nerror: %s\n%!" line e;
               line_loop parsed_lines
      end with
         | End_of_file -> List.rev parsed_lines
   in
   let entries = line_loop [] in
   close_in ic;
   entries
;;
