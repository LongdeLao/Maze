open Maze
open Backtracking
open Wilson
open Eller
open Bench_timer

let time f =
  let t0 = bench_now_ns () in
  let res = f () in
  let t1 = bench_now_ns () in
  (res, t1 -. t0)

let generate maze algo =
  match String.lowercase_ascii algo with
  | "wilson" | "w" -> Wilson.generate maze ()
  | "eller" | "e" -> Eller.generate maze ()
  | "dfs" | "backtracking" | "b" -> Backtracking.generate maze ()
  | _ -> failwith "Unknown algorithm"

let run size algo =
  let maze = Maze.create_maze size size in
  let (_generated_maze, gen_t) = time (fun () -> generate maze algo) in
  Printf.printf "Generation time: %.3f ms\n%!" (gen_t /. 1e6);
  ()

let () =
  match Array.to_list Sys.argv with
  | _ :: size_str :: algo :: _ ->
      (try
         let size = int_of_string size_str in
         run size algo
       with Failure _ -> prerr_endline "Size must be an integer")
  | _ ->
      prerr_endline "Usage: dune exec Maze_bench -- <size> <algorithm>" 