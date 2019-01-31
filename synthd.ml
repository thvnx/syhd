(******************************************************************************)

(** Main code for Synth-HD program. *)

module SYC = Syscalls
module CMD = Commandline
module EXP = Exploration
module SYN = Synth0

(******************************************************************************)

let _ =
  let start = Unix.gettimeofday() in                           (** Start time *)
  CMD.scan_cmd_line;                             (** Process the command line *)



  (********************************)
  (** Run the prospection process *)

  if !CMD.prospect then
    begin
    List.iter (fun i -> EXP.preprocess i) !CMD.files;
    end;

  if !CMD.manual_mode then
    begin
    List.iter (fun i -> EXP.process i !CMD.cohd_args) !CMD.files;
    end;

  if !CMD.devel_mode then
    begin
    List.iter (fun i -> EXP.accuracy_process i 3) !CMD.files;
    end;

  if !CMD.code_synthesis then
    begin
      List.iter (fun i -> SYN.synth0 i) !CMD.files;
    end;
  
  (********************************)


  let stop = Unix.gettimeofday() in                             (** Stop time *)
  (** Print execution time of Synth-HD *)
  let exe_time = ref (stop -. start) in
  print_string ("SyHD total time: " ^ 
  begin
    if !exe_time > 60.0 
    then 
      (string_of_float (!exe_time /. 60.0)) ^ " minutes.\n"
    else 
      (string_of_float !exe_time) ^ " seconds.\n"
  end)
  
