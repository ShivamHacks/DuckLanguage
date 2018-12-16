(* To compile: ocamlopt -o test_cmdargs unix.cmxa test_cmdargs.ml *)

open Printf
let std_out = stdout;;
open Unix
    
(*
let () =
	for i = 0 to Array.length Sys.argv - 1 do
		printf "[%i] %s\n" i Sys.argv.(i)
	done;;
*)

let rec loading arr = match arr with
	[] -> ()
	| c :: [] -> printf "%s\n" c; Unix.sleepf 0.5; flush std_out
	| c :: t -> printf "%s..." c; Unix.sleepf 0.5; flush std_out; loading t;
;;
loading ["q"; "u"; "a"; "c"; "k"];;