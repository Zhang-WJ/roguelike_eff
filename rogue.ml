open Effect
open Effect.Deep
open Notty
open Notty_unix

type cell =
  | Empty
  | Cactus
  | Camel
  | Snake
  | Elephant
  | Spider
  | Spider_egg

type world = cell array array

let width, height = 50, 30
let world = Array.make_matrix width height Empty

let get (x, y) =
  try world.(x).(y) with
  | _ -> Empty
;;

let is_valid_position (x, y) = x >= 0 && x < width && y >= 0 && y < height
let safe_get (x, y) = if is_valid_position (x, y) then world.(x).(y) else Empty
let set (x, y) v = if is_valid_position (x, y) then world.(x).(y) <- v

(* *)
let () = Random.self_init ()
let random_position () = Random.int width, Random.int height

let () =
  for _ = 0 to 200 do
    set (random_position ()) Cactus
  done;
  for _ = 0 to 20 do
    set (random_position ()) Snake
  done;
  for _ = 0 to 10 do
    set (random_position ()) Elephant
  done;
  for _ = 0 to 3 do
    set (random_position ()) Spider
  done
;;

let string_of_cell = function
  | Empty -> "  "
  | Cactus -> "\u{1F335}"
  | Camel -> "\u{1F42A}"
  | Snake -> "\u{1F40D}"
  | Elephant -> "\u{1F418}"
  | Spider -> "\u{1F577} "
  | Spider_egg -> "\u{1F95A}"
;;

let draw_cell c = I.string A.empty (string_of_cell c)

let draw_world () =
  I.hcat
  @@ Array.to_list
  @@ Array.map (fun column -> I.vcat @@ Array.to_list @@ Array.map draw_cell column) world
;;

let camel_initial_position = random_position ()
let () = set camel_initial_position Camel
let terminal = Term.create ()
let render () = Term.image terminal (draw_world ())

let keyboard_direction () =
  match Term.event terminal with
  | `Key (`Escape, _) -> exit 0
  | `Key (`Arrow `Left, _) -> -1, 0
  | `Key (`Arrow `Right, _) -> 1, 0
  | `Key (`Arrow `Up, _) -> 0, -1
  | `Key (`Arrow `Down, _) -> 0, 1
  | _ -> 0, 0
;;

let ( ++ ) (x, y) (dx, dy) = x + dx, y + dy

type _ Effect.t += Sleep : int -> unit Effect.t
type time = int

module Timeline = Binary_heap.Make (struct
    type t = time * (unit -> unit)

    let compare (t0, _) (t1, _) = Int.compare t0 t1
  end)

let dummy = 0, fun () -> assert false
let queue = Timeline.create ~dummy 32
let now = ref 0

let effect_handler =
  { effc =
      (fun (type c) (eff : c Effect.t) ->
        match eff with
        | Sleep duration ->
          Some
            (fun (k : (c, _) continuation) ->
              Timeline.add queue (!now + duration, fun () -> continue k ()))
        | _ -> None)
  }
;;

let player character = try_with character () effect_handler

let move old_positon new_position =
  if is_valid_position new_position
  then (
    match get new_position with
    | Empty ->
      let character = get old_positon in
      set old_positon Empty;
      set new_position character;
      new_position
    | _ -> old_positon)
  else old_positon
;;

let rec camel current_position =
  let new_position = current_position ++ keyboard_direction () in
  let new_position = move current_position new_position in
  perform (Sleep 1);
  camel new_position
;;

let run_queue () =
  while true do
    render ();
    let time, suspended_character = Timeline.pop_minimum queue in
    now := time;
    suspended_character ()
  done
;;

(* non-player charactor random move *)

let all_directions = [ 1, 0; -1, 0; 0, 1; 0, -1 ]

let random_direction () =
  List.nth all_directions (Random.int @@ List.length all_directions)
;;

let random_move old_positon =
  let new_position = move old_positon (old_positon ++ random_direction ()) in
  perform (Sleep 1);
  new_position
;;

let snake initional_position : unit =
  let pos = ref initional_position in
  while true do
    pos := random_move !pos
  done
;;

(* elephant's charging behavior *)
let rec camel_in_sight pos direction =
  let next_pos = pos ++ direction in
  if is_valid_position next_pos
  then false
  else (
    (* check if the next position is a camel *)
    match get next_pos with
    | Camel -> true
    | Empty -> camel_in_sight next_pos direction
    | _ -> false)
;;

let camel_in_sight pos = List.find_opt (camel_in_sight pos) all_directions

let rec elephant pos : unit =
  match camel_in_sight pos with
  | None -> elephant (random_move pos)
  | Some direction ->
    let rec charge pos =
      let next_pos = pos ++ direction in
      match get next_pos with
      | Empty ->
        let next_pos = move pos next_pos in
        perform (Sleep 1);
        charge next_pos
      | Cactus ->
        perform (Sleep 20);
        pos
      | _ ->
        perform (Sleep 1);
        pos
    in
    let pos_after = charge pos in
    elephant pos_after
;;

let spawn child = Timeline.add queue (!now, fun () -> player child)

let rec spider pos =
  try_to_lay_egg pos;
  let new_position = random_move pos in
  spider new_position

and try_to_lay_egg pos =
  let egg_position = pos ++ random_direction () in
  if get egg_position = Empty && Random.int 100 = 0
  then (
    set egg_position Spider_egg;
    spawn (fun () -> egg egg_position))

and egg pos =
  perform (Sleep 10);
  for dx = -1 to 1 do
    for dy = -1 to 1 do
      let new_position = pos ++ (dx, dy) in
      if get new_position = Empty
      then (
        set new_position Spider;
        spawn (fun () -> spider new_position))
    done
  done
;;

let _ =
  world
  |> Array.iteri
     @@ fun x ->
     Array.iteri
     @@ fun y -> function
     | Camel -> spawn (fun () -> camel (x, y))
     | Spider -> spawn (fun () -> spider (x, y))
     | Elephant -> spawn (fun () -> elephant (x, y))
     | Snake -> spawn (fun () -> snake (x, y))
     | _ -> ()
;;

(*  ocamlfind ocamlopt -package notty,notty.unix,unix -linkpkg lib/roguelike.ml -o roguelike   *)
let () = run_queue ()
