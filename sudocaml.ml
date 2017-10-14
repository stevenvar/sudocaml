(* Sudoku solver (based on Peter Norvig article http://norvig.com/sudoku.html) *)

type digit = string
type square = string
type unit = square list

type grid = (square,(digit list)) Hashtbl.t

exception Empty of square

let cross a b =
  List.map (fun x ->
      List.map (fun y -> x^y) b) a |> List.flatten


let digits = ["1";"2";"3";"4";"5";"6";"7";"8";"9"]
let rows = ["A";"B";"C";"D";"E";"F";"G";"H";"I"]

let cols = digits
let squares = cross rows cols

let box_list =
  List.map (fun x -> List.map (fun y -> cross y x)
               [["A";"B";"C"];
                ["D";"E";"F"];
                ["G";"H";"I"]])
    [["1";"2";"3"];
     ["4";"5";"6"];
     ["7";"8";"9"]] |> List.flatten

let (unitlist: unit list) =
  let row_list = List.map (fun x -> cross rows [x]) cols in
  let col_list = List.map (fun x -> cross [x] cols) rows in
  row_list@col_list@box_list

let units =
  let unit_of_square s = List.filter (List.mem s) unitlist in
  List.map (fun x -> (x,unit_of_square x)) squares

let peers =
  let peers_of_square s =
    let l = List.assoc s units in
    let ll = List.flatten l in 
    List.filter (fun x -> x <> s) ll
  in
  List.map (fun x -> (x,peers_of_square x)) squares

let grid_values s =
  assert (String.length s = 81);
  let list_of_string s =
    let rec los i l =
      if i < 0 then l
      else los (i-1) (String.make 1 s.[i] :: l) 
    in
    los (String.length s -1) []
  in
  List.combine squares (list_of_string s)


let print_grid (g:grid) =
  let line = "------+-------+-------" in 
  List.iter (
    fun r ->
      List.iter (fun c ->
          let digits = Hashtbl.find g (r^c) in
          List.iter print_string digits;
          if c = "3" || c = "6" then
            print_string " | "
          else
            print_string " ";
        ) cols ;
      print_newline ();
      if r = "C" || r = "F" then print_endline line;

  ) rows


let rec assign (values:grid) s d =
  let other_values = Hashtbl.find values s in
  let other_values = List.filter (fun x -> x <> d) other_values in
  List.iter (fun d2 -> eliminate values s d2) other_values;

and eliminate values s d = 
  let vl = Hashtbl.find values s in
  if (List.mem d vl) then
    begin
      (* values[s] = values[s] - d  *)
      Hashtbl.replace values s (List.filter (fun x -> x <> d) vl);
      (match Hashtbl.find values s with
       | [] -> raise (Empty s);
       | [d2] ->
         List.iter (fun s2 -> eliminate values s2 d2) (List.assoc s peers)
       | _ -> ());
      let unit_s = List.assoc s units in 
      List.iter (fun u ->
          (* for each square u in the same unit as s  *)
          (* for each s' in u, if d is in possibles values of s'  *)
          let dplaces = List.filter (fun s' ->
              let vofs = Hashtbl.find values s' in
              List.mem d vofs
            ) u in

          match dplaces with
          (* No place for the value (contradiction) *)
          | [] -> raise (Empty d)
          (* there is only one place for d in this unit  *)
          | [x] ->
            assign values x d
          | _ -> ()
        ) unit_s
    end

let parse_grid s =
  let all = Hashtbl.create 81 in 
  List.iter (fun x -> Hashtbl.add all x digits) squares;
  List.iter (
    fun (s,d) ->
      if d <> "0" && d <> "." then
        assign all s d
  ) (grid_values s);
  all

let rec search values =
  (* if all possibles values are unique, its solved *)
  if List.for_all
      (fun s -> List.length (Hashtbl.find values s) = 1) squares then
      Some values 
  else
    (* Chose the unfilled square s with the fewest possibilities :  *)
    (* get squares with at least 2 possibilities *)
    let l = List.filter (fun s -> List.length (Hashtbl.find values s) > 1) squares in
    let s_begin = List.hd l in
    let min_begin = Hashtbl.find values s_begin |> List.length in
    let (n,s) = List.fold_left (fun (min,curs) s ->
        let cpt = List.length (Hashtbl.find values s) in
        if cpt < min then (cpt,s) else (min,curs)) (min_begin,s_begin)  l in
    (* try each possible value *)
    List.fold_left (fun acc d -> 
        try
          (* Printf.printf "I try to put %s in %s \n" d s ; *)
          let copy = Hashtbl.copy values in 
          assign copy s d;
          match search copy with
          | Some x -> Some x
          | None -> acc
        with Empty _ -> acc         
      ) None (Hashtbl.find values s)


let solve g =
  search (parse_grid g)


let file_to_string file =
  let ic = open_in file in
  let s = ref "" in
  try
    while true do 
      s := !s ^ (input_line ic);
    done ; !s
  with End_of_file -> !s

let _ =
  if Array.length Sys.argv < 2 then
    print_string "usage : sudocaml file.txt \n"
  else
    let g = (file_to_string (Sys.argv.(1))) in
    let k = solve g in
    match k with
    | None -> print_string "No solution found :( \n"
    | Some k -> print_grid k
