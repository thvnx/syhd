(******************************************************************************)

(** Launch system call. *)

open Config

module CMD = Commandline


(******************************************************************************)

(** Run a command 
    @param cmd the command to run *)
let command cmd =
  if !CMD.verbose then print_endline ("[CMD] " ^ cmd);
  if Sys.command cmd <> 0 then
    failwith ("Error during: " ^ cmd ^ "\n")


(** Launch Co-HD program
    @param file C file to run in Co-HD
    @param args Arguments for Co-HD *)
let launch_cohd file args =
  let cmd = (Config.cohd_path ^ " " ^ file ^ " " ^ args) in
  command cmd


(** Launch the Compiler
    @param file C file to run in the compiler
    @param args Arguments for the compiler *)
let compil_c file args =
  let args2 = 
    if !CMD.args_is_set then !CMD.comp_opt
    else args
  in
  let cmd = (!CMD.compiler ^ " " ^ file ^ " " ^ args2) in
  command cmd
