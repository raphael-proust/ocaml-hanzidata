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

module Parsing : sig

   val entry_of_string: string -> entry

end = struct

   (* Error management helpers *)
   let wild s j = Helpers.Parsing.handle_other_cases s j
   let unopt name v = match v with
      | Some v -> v
      | None -> raise (Helpers.Parsing.Error ("Missing field " ^ name))
   ;;


   (* Decoders for individual fields *)
   let parse_character =
      let open Helpers.Parsing in
      string "Expected string value for character field" >|= fun s ->
      let udecoder = Uutf.decoder (`String s) in
      match Uutf.decode udecoder with
      | `Uchar u -> begin
         match Uutf.decode udecoder with
         | `End -> u
         | `Uchar _ -> raise (Helpers.Parsing.Error "Expected single uchar in character field")
         | `Malformed _ -> raise (Helpers.Parsing.Error "Malformed unicode string in character field")
         | `Await -> assert false
         end
      | `End -> raise (Helpers.Parsing.Error "Expected uchar in character field")
      | `Malformed _ -> raise (Helpers.Parsing.Error "Malformed unicode string in character field")
      | `Await -> assert false
   ;;

   let parse_definition =
      Helpers.Parsing.string "Expected string in definition field"

   let parse_pinyin jdecoder =
      match Jsonm.decode jdecoder with
      | `Lexeme `As as las ->
         Helpers.Parsing.array
            (fun l _ -> match l with
               | `Lexeme (`String py) -> py
               | j -> wild "Unexpected value in pinyin array" j
            )
            las jdecoder
      | j -> wild "Expected array in pinyin field" j
   ;;

   let parse_decomposition_string s =
      let udecoder = Uutf.decoder (`String s) in
      let get_uchar () =
         match Uutf.decode udecoder with
         | `Uchar u -> u
         | `Malformed _ -> raise (Helpers.Parsing.Error "Malformed unicode string in decomposition field")
         | `End -> raise (Helpers.Parsing.Error "Unexpected end of decomposition string")
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
   let parse_decomposition =
      let open Helpers.Parsing in
      string "Expected string value in decomposition field" >|= fun s ->
      parse_decomposition_string s
   ;;

   let parse_radical =
      let open Helpers.Parsing in
      string "Expected string value in radical field" >|= fun s ->
      let udecoder = Uutf.decoder (`String s) in
      match Uutf.decode udecoder with
      | `Uchar u -> begin
         match Uutf.decode udecoder with
         | `End -> u
         | `Uchar _ -> raise (Helpers.Parsing.Error "Expected single uchar in radical field")
         | `Malformed _ -> raise (Helpers.Parsing.Error "Malformed unicode string in radical field")
         | `Await -> assert false
         end
      | `End -> raise (Helpers.Parsing.Error "Expected uchar in radical field")
      | `Malformed _ -> raise (Helpers.Parsing.Error "Malformed unicode string in radical field")
      | `Await -> assert false
   ;;

   let parse_etymology =
      let open Helpers.Parsing in
      eat `Os "Expected string value in radical field" >>= fun () ->
      let rec loop kv jdecoder =
         match Jsonm.decode jdecoder with
         | `Lexeme `Oe -> kv
         | `Lexeme (`Name k) -> begin
            match Jsonm.decode jdecoder with
            | `Lexeme (`String v) -> loop ((k, v) :: kv) jdecoder
            | j -> wild "Expected string value in etymology entry" j
            end
         | j -> wild "Expected a dictionary in etymology field" j
      in
      loop []
   ;;

   let parse_match las jdecoder =
      Helpers.Parsing.array
      (fun l jdecoder -> match l with
         | `Lexeme (`Float f) -> int_of_float f
         | j -> wild "Expected numbers in matches array array" j
      )
      las jdecoder
   ;;
   let parse_matches jdecoder =
      match Jsonm.decode jdecoder with
      | `Lexeme `As as las ->
         Helpers.Parsing.array
            (fun l jdecoder -> match l with
            | `Lexeme `As as las -> Some (parse_match las jdecoder)
            | `Lexeme `Null -> None
            | j -> wild "Expected arrays in matches array" j
            )
            las jdecoder
      | j -> wild "Expected array in matches field" j
   ;;


   let entry_of_string s =

      (* Set up *)
      let character = ref None in
      let definition = ref None in
      let pinyin = ref None in
      let decomposition = ref None in
      let etymology = ref None in
      let radical = ref None in
      let matches = ref None in
      (* Parser for a whole entry *)
      let rec main_parser : unit Helpers.Parsing.t = fun jdecoder ->
         match Jsonm.decode jdecoder with
         | `Lexeme (`Name "character") ->
            character := Some (parse_character jdecoder);
            main_parser jdecoder
         | `Lexeme (`Name "definition") ->
            definition := Some (parse_definition jdecoder);
            main_parser jdecoder
         | `Lexeme (`Name "pinyin") ->
            pinyin := Some (parse_pinyin jdecoder);
            main_parser jdecoder
         | `Lexeme (`Name "decomposition") ->
            decomposition := Some (parse_decomposition jdecoder);
            main_parser jdecoder
         | `Lexeme (`Name "etymology") ->
            etymology := Some (parse_etymology jdecoder);
            main_parser jdecoder
         | `Lexeme (`Name "radical") ->
            radical := Some (parse_radical jdecoder);
            main_parser jdecoder
         | `Lexeme (`Name "matches") ->
            matches := Some (parse_matches jdecoder);
            main_parser jdecoder
         | `Lexeme `Oe -> begin
            match Jsonm.decode jdecoder with
            | `End -> ()
            | j -> wild "Expected end of string after end of object" j
            end
         | j -> wild "Unexpected lexeme in entry" j
      in

      (* Actual parsing *)
      Helpers.Parsing.(
         run
            (eat `Os  "Expected object start at start of string" >>= fun () ->
             main_parser)
            (Jsonm.decoder (`String s))
      );

      (* Post-process *)
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

end


let read ?(path = "./makemeahanzi/dictionary.txt") () =
   let ic = open_in path in
   let rec line_loop parsed_lines =
      try begin
         let line = input_line ic in
         try
            let parsed_line = Parsing.entry_of_string line in
            line_loop (parsed_line :: parsed_lines)
         with
            | Helpers.Parsing.Error e ->
               Printf.fprintf stderr "line: %s\nerror: %s\n%!" line e;
               line_loop parsed_lines
      end with
         | End_of_file -> List.rev parsed_lines
   in
   let entries = line_loop [] in
   close_in ic;
   entries
;;
