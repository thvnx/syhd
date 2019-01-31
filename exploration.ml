(******************************************************************************)

(** Run program to get starting informations : accuracy and performances of the 
    original and double-double overloaded program *)

open Config

module SYS = Syscalls
module DAT = Datamanager
module CMD = Commandline
module PLT = Plot


let search_max_hashtbl tbl =
  let max_key = ref "" in
  let max_val = ref 0.0 in
  Hashtbl.iter (
    fun i j ->
      if j > !max_val then
	begin
	  max_key := i;
	  max_val := j;
	end
  ) tbl;
  !max_key

let int_search_max_hashtbl tbl =
  let max_key = ref "" in
  let max_val = ref 0 in
  Hashtbl.iter (
    fun i j ->
      if j > !max_val then
	begin
	  max_key := i;
	  max_val := j;
	end
  ) tbl;
  !max_key


(******************************************************************************)

(** Create data list and run Co-HD to get informations about accuracy and
    performances.
    @param file C name file to study
    @param args Arguments for Co-HD
    @return Data with results *)
let explore file args =
  let data = DAT.read_data_file !CMD.data_file in      (** Read the data file *)
  SYS.launch_cohd file (args ^ " -pa");             (** Lauch Co-HD with args *)
  SYS.compil_c Config.tmp_cohd_file Config.gcc_options;           (** Run GCC *)

  (** Run the given program *)
  (** First, to get informations about accuracy *)
  List.iter (
    fun i ->
      i#execute_data Config.tmp_exe
  ) data;

                                                    (** Lauch Co-HD with args *)
  SYS.launch_cohd file (args ^ " -pp -pn " ^ (string_of_int !nb_iter));
  SYS.compil_c Config.tmp_cohd_file Config.gcc_options;           (** Run GCC *)

  (** Then, to get informations about performances *)
  let data_str = (List.hd data)#get_data in
  let perf_data = (new DAT.data_perfs (data_str)) in
  perf_data#execute_data Config.tmp_exe;

  (** True if the data is one numerical value *)
  let numerical = String.rcontains_from data_str ((String.length data_str)-2) ' ' in
  (data, perf_data, not numerical)


(******************************************************************************)

(** PreProcess the program to get informations
    @param file C name file to study *)
let preprocess file = 

  (**********************************)
  (** Original program informations *)

  (** Arguments for Co-HD :
      -o <res file> 
      -pp to preprocess the pragma 
      -tac to generate a 3-address code *)
  let args = "-o " ^ Config.tmp_cohd_file ^ " -tac" in
  let (prec_or, perf_or, _) = explore file args in


  (**********************************************)
  (** Double-precision overloading informations *)

  (** Arguments for Co-HD :
      -o <res file> 
      -pp to preprocess the pragma 
      -hd and -dd to overload the floating-point operators with 
         double-precision operators *)
  let args = "-o " ^ Config.tmp_cohd_file ^ " -hd -dd" in
  let (prec_dd, perf_dd, num) = explore file args in

  (** Plot the results *)
  PLT.plot2 (String.sub file 0 (String.rindex file '.')) 
                                          prec_or perf_or prec_dd perf_dd num;

  (************************************)
  (** Clear tmp files of this process *)
  Sys.remove Config.tmp_file;
  Sys.remove Config.tmp_cohd_file;
  Sys.remove Config.tmp_exe;
  Sys.remove Config.tmp_plot




(******************************************************************************)

(** Process the program to get informations
    @param file C name file to study
    @param args Co-HD arguments*)
let process file cohd_args = 

  (**********************************)
  (** Original program informations *)

  (** Arguments for Co-HD :
      -o <res file> 
      -tac to generate a 3-address code *)
  let args = "-o " ^ Config.tmp_cohd_file ^ " -tac" in
  let (prec_or, perf_or, _) = explore file args in


  (**********************************************)
  (** Manual Co-HD informations *)

  (** Arguments for Co-HD :
      -o <res file> 
      arguements given by the user *)
  let args = "-o " ^ Config.tmp_cohd_file ^ " " ^ cohd_args in
  let (prec_mn, perf_mn, _) = explore file args in


  (**********************************************)
  (** Double-precision overloading informations *)

  (** Arguments for Co-HD :
      -o <res file> 
      -hd and -dd to overload the floating-point operators with 
         double-precision operators *)
  let args = "-o " ^ Config.tmp_cohd_file ^ " -hd -dd" in
  let (prec_dd, perf_dd, num) = explore file args in


  (** Plot the results *)
  let without_extension = (String.sub file 0 (String.rindex file '.')) in
  let filename =
  try (
    let idx = (String.index without_extension '/') + 1 in
    String.sub without_extension (idx) (String.length without_extension - idx)
  )
  with Not_found -> without_extension
  in

  PLT.plot3 filename
                            prec_or perf_or prec_mn perf_mn prec_dd perf_dd num;

  (************************************)
  (** Clear tmp files of this process *)
  Sys.remove Config.tmp_exe;
  Sys.remove Config.tmp_file;
  Sys.remove Config.tmp_cohd_file;
(*  Sys.remove Config.tmp_plot
*)
  ()

(******************************************************************************)
(******************************************************************************)

(** Create data list and run Co-HD to get informations about accuracy.
    @param file C name file to study
    @param number number of variable to transform
    @return Args list results *)
let accuracy_explore file number =

                                        (** Read the data file and build data *)
  let data = DAT.read_data_file !CMD.data_file in

                                                    (** Lauch Co-HD with args *)
  SYS.launch_cohd file ("-o " ^ Config.tmp_cohd_file ^ " -tac -pi");
  SYS.compil_c Config.tmp_cohd_file Config.gcc_options;           (** Run GCC *)

  (** First, run the given original program *)
  List.iter (
    fun i ->
      i#execute_data_or Config.tmp_exe
  ) data;

                                                    (** Lauch Co-HD with args *)
  SYS.launch_cohd file ("-o " ^ Config.tmp_cohd_file ^ " -tac -pi -hd -dd");
  SYS.compil_c Config.tmp_cohd_file Config.gcc_options;           (** Run GCC *)

  (** Then, run the given program with double-double *)
  List.iter (
    fun i ->
      i#execute_data_dd Config.tmp_exe
  ) data;
 
  (** compute the error *)
  let args_list = Hashtbl.create 0 in
  let add_item tbl i =
    if Hashtbl.mem tbl i then
      Hashtbl.replace tbl i ((Hashtbl.find tbl i)+1)
    else Hashtbl.add tbl i 1
  in  
  List.iter (
    fun i ->
      let res = i#compute_errors () in
      for n = 1 to number do
	let max = search_max_hashtbl res in
	if max <> "" then
	  begin
	    Hashtbl.remove res max;
	    add_item args_list max;
	  end
      done;
  ) data;

  (* Hashtbl.iter ( *)
  (*   fun i j -> print_string (i ^ " " ^ string_of_int j ^ "\n") *)
  (* ) args_list; *)

  let str = ref "" in
  for n = 1 to (min number (Hashtbl.length args_list)) do
    let max = int_search_max_hashtbl args_list in
    Hashtbl.remove args_list max;
    str := !str ^ max ^ ",";
  done;
  str := String.sub !str 0 ((String.length !str)-1);  

  !str


(** Process the program to get informations about accuracy.
    @param file C name file to study
    @param number number of variable to return *)
let accuracy_process file number = 
  let variables = accuracy_explore file number in
  let args = " -t2 " ^ variables in
  process file args


