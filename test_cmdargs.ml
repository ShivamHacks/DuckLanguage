(* To compile: ocamlopt -o test_cmdargs unix.cmxa test_cmdargs.ml *)

open Printf
let std_out = stdout;;
open Unix
open Scanf

let duck = "🦆";

let rec loading arr = match arr with
	[] -> ()
	| c :: [] -> printf "%s\n" c; Unix.sleepf 0.5; flush std_out
	| c :: t -> printf "%s..." c; Unix.sleepf 0.5; flush std_out; loading t;
;;
loading ["q"; "u"; "a"; "c"; "k"];;

let read_all_lines file_name =
	let ic = open_in file_name in
	let ib = Scanning.from_channel ic in
	let rec read_recursive file =
	try
		Scanf.bscanf ib "%[^\r\n]\n" (fun x -> read_recursive (file ^ "\n" ^ x))
	with
		End_of_file -> file in
	let file = read_recursive "" in
	let _ = close_in_noerr ic in file
;;

printf "%s" (read_all_lines Sys.argv.(1));;
