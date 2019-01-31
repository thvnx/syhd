(******************************************************************************)

(** Run program to get source code with accuracy-performances
    improvements. Synth0 is a first naive search algorithm to code
    synthesis *)

open Config
module EXP = Exploration
module DAT = Datamanager
module CMD = Commandline
module SYS = Syscalls



(******************************************************************************)

(** Partially compensated program object *)

class pcp ((args:string), (acc:DAT.data list), (perf:DAT.data_perfs), (num:bool)) =
object(self)
  
  val cohd_arguments = args
  val data_accuracy = acc
  val data_performances = perf
  val data_numerical = num

  method get_data_accuracy = data_accuracy
  method get_cohd_arguments = cohd_arguments


  method get_nb_cycles = int_of_string (data_performances#get_cycles)
  method get_ratio_cycles (refc:pcp) = float_of_int(self#get_nb_cycles) /. float_of_int(refc#get_nb_cycles)


  method nb_cycles_to_string () = data_performances#get_cycles
  method ratio_cycles_to_string (refc:pcp) = string_of_float(self#get_ratio_cycles(refc))


  method get_nb_bits (refc:pcp) =
    let (_, bit) = DAT.compute_errors data_accuracy refc#get_data_accuracy in
    if Array.length bit > 1 then
      begin
	prerr_string "errorhere";
	-1
      end
    else
      begin
	bit.(0)
      end
  method get_ratio_bits (refc:pcp) =
    float_of_int (self#get_nb_bits(refc)) /. float_of_int !Config.ddbe

  method nb_bits_to_string (refc:pcp) = string_of_int (self#get_nb_bits(refc))
  method ratio_bits_to_string (refc:pcp) = string_of_float (self#get_ratio_bits (refc))



  method r1 (refc:pcp) =
    let alp         = float_of_int !Config.alpha in
    let bet         = float_of_int !Config.beta in
    (alp *. self#get_ratio_cycles (refc) +. bet *. (1. -. self#get_ratio_bits (refc))) /. (alp +. bet)

  method r2 (refc:pcp) =
    let alp         = float_of_int !Config.alpha in
    let bet         = float_of_int !Config.beta in
    abs_float (bet *. self#get_ratio_bits (refc) -. alp *. self#get_ratio_cycles (refc))

  method to_string (refc:pcp) =
    "\"" ^ cohd_arguments ^ "\" " ^ self#nb_cycles_to_string () ^ " " ^ self#ratio_cycles_to_string (refc)
  ^ " " ^ self#nb_bits_to_string (refc) ^ " " ^ self#ratio_bits_to_string (refc)


end
  
(** This search algorithm will compute a finite fixed set of partially
    compensated programs described in the following table *)

let nuscale i =
  string_of_int (i * (!Config.nu / 10))


let t0_args_set =
  "-t0 -n 2 -m 5" :: (* 40% *)
  "-t0 -n 1 -m 2" :: (* 50% *)
  "-t0 -n 3 -m 5" :: (* 60% *)
  "-t0 -n 7 -m 10" :: (* 70% *)
  "-t0 -n 4 -m 5" :: (* 80% *)
  "-t0 -n 9 -m 10" :: (* 90% *)
  [] ;;

let t1_args_set =
  [("-t1 -n "^(nuscale 1)^" -m "^(nuscale 5));
  "-t1 -n "^(nuscale 2)^" -m "^(nuscale 5) ;
  "-t1 -n "^(nuscale 3)^" -m "^(nuscale 5) ;
  "-t1 -n "^(nuscale 4)^" -m "^(nuscale 5) ;
  "-t1 -n "^(nuscale 1)^" -m "^(nuscale 2) ;
  "-t1 -n "^(nuscale 1)^" -m "^(nuscale 3) ;
  "-t1 -n "^(nuscale 1)^" -m "^(nuscale 4) ;
  "-t1 -n "^(nuscale 2)^" -m "^(nuscale 3) ;
  "-t1 -n "^(nuscale 2)^" -m "^(nuscale 4) ; 
  "-t1 -n "^(nuscale 3)^" -m "^(nuscale 4) ;
  ] ;;


let synth0 file =
  let args_set = ref [] in

  (* Set the t0 and t1 transformation arguments *)
  List.iter (
    fun i -> args_set := !args_set @ [(i);(i ^ " -p");(i ^ " -e")]
  ) t0_args_set;
  List.iter (
    fun i -> args_set := !args_set @ [(i);(i ^ " -p");(i ^ " -e");(i ^ " -p -e")]
  ) t1_args_set;

  (* Compute the t2 arguments for n<=4 *)
  (* List.iter ( *)
  (*   fun i ->  *)
  (*     let args = EXP.accuracy_explore file i in *)
  (*     args_set := !args_set @ [("-t2 " ^ args)] *)
  (* ) [1]; (\*[1;2;3;4];*\) *)

  for i = 1 to !Config.t2limit do
      let args = EXP.accuracy_explore file (int_of_string (nuscale i)) in
      args_set := !args_set @ [("-t2 " ^ args)]
  done;

  let args_set2 = ref [("-hd");("-tac")] in

  (* Build and execute all the compensated programs *)
  let results = ref [] in (*Hashtbl.create 0 in*)
  List.iter (
    fun i -> 
      (*print_endline i;*)
      let args = "-o " ^ Config.tmp_cohd_file ^ " " ^ i in
      let (acc, per, num) = EXP.explore file args in
      results := !results @[ (new pcp(i, acc, per, num)) ];
  ) !args_set;

  let results2 = ref [] in 
  List.iter (
    fun i -> 
      let args = "-o " ^ Config.tmp_cohd_file ^ " " ^ i in
      let (acc, per, num) = EXP.explore file args in
      results2 := !results2 @[ (new pcp(i, acc, per, num)) ];
  ) !args_set2;

  (*print_int (List.length !args_set)*)

  let args_ref = "-o " ^ Config.tmp_cohd_file ^ " -hd -dd" in
  let (acc_ref, per_ref, num_ref) = EXP.explore file args_ref in
  let res_ref = new pcp(args_ref, acc_ref, per_ref, num_ref) in

  let winner = ref [] in
  winner := [(List.hd !results)];

  let ch = open_out ((String.sub !CMD.out_file 0 (String.rindex !CMD.out_file '.')) ^ ".s") in
  List.iter (
    fun j ->
      (*let nb_bit_j    = j#nb_bits_to_string(res_ref) in
      let nb_cycles_j = j#nb_cycles_to_string () in
      let ra_cycles_j = j#ratio_cycles_to_string(res_ref) in
      let ra_bit_j    = j#ratio_bits_to_string(res_ref) in*)
      

      let ra_pp = 
	if !CMD.code_spec = "perf1" then
	  begin
	    let ratio = j#r1(res_ref) in
	    if ((List.hd !winner)#r1(res_ref)) > ratio then
	      winner := [(j)];

	    string_of_float ratio
	  end
	else
	  begin
	    if !CMD.code_spec = "perf2" then
	      begin
		let ratio = j#r2(res_ref) in
		if ((List.hd !winner)#r2(res_ref)) < ratio then
		  winner := [(j)];
		
		string_of_float ratio
	      end
	    else 
	      begin
		let bitr = (j#get_ratio_bits (res_ref)) in
		if bitr = ((List.hd !results2)#get_ratio_bits (res_ref)) then
		  begin  
		  if ((List.hd !winner)#get_ratio_cycles (res_ref)) > (j#get_ratio_cycles (res_ref)) then
		    begin  winner := [(j)]; end
		  else begin
		  winner := [(List.hd !results2)]; end;
		end
		else
		  begin
	            winner := [(List.hd !results2)]; 
		  end;
		"x"
	      end
	  end
      in

      output_string ch (j#to_string (res_ref) ^ " " ^ ra_pp ^ "\n");

  ) !results;

  for i = 1 to 4 - !Config.t2limit do
    output_string ch "sp - - - - -\n";
  done;

  List.iter (
    fun j ->

      let ra_pp = 
	if !CMD.code_spec = "perf1" then
	  begin
	    let ratio = j#r1(res_ref) in
	    string_of_float ratio
	  end
	else
	  begin
	    if !CMD.code_spec = "perf2" then
	      begin
		let ratio = j#r2(res_ref) in
		string_of_float ratio
	      end
	    else 
	      begin
		"x"
	      end
	  end
      in

      output_string ch (j#to_string (res_ref) ^ " " ^ ra_pp ^ "\n");

  ) !results2;

  output_string ch ("\n# " ^ (List.hd !winner)#to_string (res_ref) ^ "\n");

  let ch_cohd = open_out ((String.sub !CMD.out_file 0 (String.rindex !CMD.out_file '.')) ^ ".cohd") in
  output_string ch_cohd ((List.hd !winner)#get_cohd_arguments);
  close_out ch_cohd;

  close_out ch;


  let args_win = "-o " ^ !CMD.out_file ^ " " ^ (List.hd !winner)#get_cohd_arguments in
  SYS.launch_cohd file args_win;             (** Lauch Co-HD with args *)


  Sys.remove Config.tmp_cohd_file;
  Sys.remove Config.tmp_file;
  Sys.remove Config.tmp_exe;
