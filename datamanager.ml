(******************************************************************************)

(** The data manager store data from the given data file and the results of 
    each Co-HD program execution. It provides some functions to manipulate 
    the data and the results *)

open Printf
open Config
module SYC = Syscalls

exception MyDivision_by_zero

(******************************************************************************)

(** Data class store data and provides usefull methods to manipulate them. 
    Synth-HD will create one object per data. 
    @param dat a string representation of the data
    @param line the line number of data in data file *)
class data ((dat:string), (line:int)) =
object(self)

  val data_string = dat
  val data_line   = line

  (** The execution result of the data and the given program
      (one line = one list element). *)
  val mutable data_res = []
  val mutable data_res_accuracy = []
  
  method get_data = data_string

  (** Execute the given program and save the result 
      @param exe execuble name of the given program *)
  method execute_data exe =
    let rec read_results ch =                  (** Read channel line per line *)
      try 
      begin
        data_res <- data_res @ [input_line ch];
        read_results ch
      end
      with End_of_file -> ()
    in 
    let res_file = Config.tmp_file in
    SYC.command (exe ^ " " ^ data_string ^ " > " ^ res_file);
    let chan = open_in res_file in
    read_results chan;
    close_in chan



  (** Accuracy : execute the original program and save the result 
      @param exe execuble name of the given program *)
  method execute_data_or exe =
    let rec read_results ch =                  (** Read channel line per line *)
      try 
      begin
        data_res_accuracy <- data_res_accuracy @ [(input_line ch,"")];
        read_results ch
      end
      with End_of_file -> ()
    in 
    let res_file = Config.tmp_file in
    SYC.command (exe ^ " " ^ data_string ^ " > " ^ res_file);
    let chan = open_in res_file in
    read_results chan;
    close_in chan
  (** Accuracy : execute the original program with double-double and
      save the result .
      @param exe execuble name of the given program *)
  method execute_data_dd exe =
    let data_tmp = ref [] in
    let data_tmp2 = ref [] in
    let rec read_results ch =                  (** Read channel line per line *)
      try 
      begin
        data_tmp := !data_tmp @ [input_line ch];
        read_results ch
      end
      with End_of_file -> ();
      List.iter2 (
	fun i (k,l) -> 
          data_tmp2 := !data_tmp2 @ [(k,i)];
      ) !data_tmp data_res_accuracy;
      data_res_accuracy <- !data_tmp2
    in 
    let res_file = Config.tmp_file in
    SYC.command (exe ^ " " ^ data_string ^ " > " ^ res_file);
    let chan = open_in res_file in
    read_results chan;
    close_in chan
(*;
    print_string (self#to_string ())*)
  method compute_errors () =
    let errors = Hashtbl.create 0 in
    let add_e i e = 
      if Hashtbl.mem errors i then
	begin
	  let tmp = Hashtbl.find errors i in
	  if tmp < e then
	    Hashtbl.replace errors i e;
	end
      else Hashtbl.add errors i e
    in
    let get_name str =
      Str.string_after (Str.string_before str (String.index str '=')) ((String.index str ' ')+1)
    in
    let get_val str =
      float_of_string (Str.string_after str ((String.index str '=')+1))
    in
    let absolute_error refe valu =
      abs_float ((get_val refe) -. (get_val valu))
    in 
    List.iter (
      fun (i,j) -> 
	if String.contains i '@' then
	  begin
	    add_e (get_name i) (absolute_error j i)
	  end
    ) data_res_accuracy;
    errors




  (** String representation of the accuracy data *)
  method accuracy_to_string () =
    let str = ref "" in
    List.iter (
      fun i ->
       if String.contains i '@' then
       match String.sub i 0 3 with
       | "@AC" -> 
         let j = Str.string_after i ((String.index i '=')+1) in
         str := !str ^ j ^ " "
       | _ -> ()
    ) data_res;
    !str


  (** Get the variables names of accuracy results *)
  method get_accuracy_titles () =
    let str = ref [] in
    List.iter (
      fun i ->
       if String.contains i '@' then
       match String.sub i 0 3 with
       | "@AC" ->
         let j = Str.string_after
          (Str.string_before i (String.index i '=')) ((String.index i ' ')+1) in
         str := !str @ [j]
       | _ -> ()
    ) data_res;
    !str

  (** String representation of the data *)
  method to_string () =
    let str = ref "\n***" in

    (** select the data list to print *)
    if (List.length data_res) > 0 then
      begin 
	List.iter (
	  fun i ->
	    str := !str ^ "\n" ^ i
	) data_res
      end
    else
      begin
	List.iter (
	  fun (i,j) ->
	    str := !str ^ "\n" ^ i ^ "--" ^ j
	) data_res_accuracy
      end;

    str := !str ^ "\n***\n";
    "line:" ^ (string_of_int data_line) ^ "\t" ^ data_string ^ !str

end


(******************************************************************************)

(** Data list construction. *)

let data_list = ref []
let add_data id data = 
  data_list := !data_list @ [(new data (data, id))]


(** Read a data file and create one object per data. 
    @param file file name *)
let read_data_file file =
  data_list := [];
  if file <> "" then
  begin 
    let rec read_file channel line_number =
      try
      begin 
        let line = input_line channel in
        add_data line_number line;
        read_file channel (line_number + 1)
      end
      with End_of_file -> ()
    in 
    let channel = open_in file in
    read_file channel 1;
    close_in channel 
  end
  else failwith "No data file !";
  !data_list


(******************************************************************************)

(** A special data class to store data and provides usefull methods to measure
    performances.
    @param dat a string representation of the data *)
class data_perfs ((dat:string)) =
object

  val data_string = dat

  (** The number of cycles *)
  val mutable total_cycle       = 0
  (** The number of instructions *)
  val mutable total_instruction = 0
  (** The number of floating-point instructions *)
  val mutable fp_instruction    = 0


  (** Execute the given program 
      @param exe execuble name of the given program *)
  method execute_data exe =
    (*let accum line =                               (** Accumulate thr results *)
      if String.contains line '@' then
	begin
          let s_beg = String.sub line 0 3 in
	  let s_end = String.sub line 4 ((String.length line) - 4) in 
          match s_beg with
	  | "@TC" -> 
            total_cycle       <- total_cycle       + (int_of_string s_end)
	  | "@TI" -> 
            total_instruction <- total_instruction + (int_of_string s_end)
	  | "@FP" -> 
            fp_instruction    <- fp_instruction    + (int_of_string s_end)
	  | _ -> ()
	end
    in*)
    let rec read_results ch =             (** Read results file line per line *)
      try 
        begin
          (*accum (input_line ch);*)
        let line = input_line ch in
        if String.contains line '@' then
	  begin
          let s_beg = String.sub line 0 3 in
	  let s_end = String.sub line 4 ((String.length line) - 4) in 
          match s_beg with
	  | "@TC" -> 
            total_cycle       <- (int_of_string s_end)
	  | "@TI" -> 
            total_instruction <- (int_of_string s_end)
	  | "@FP" -> 
            fp_instruction    <- (int_of_string s_end)
	  | _ -> ()
          end;
          read_results ch
        end
      with End_of_file -> ()
    in 
    let res_file = Config.tmp_file in 
    let chan     = open_in res_file in
    (*for i=1 to data_nb do*)
      SYC.command (exe ^ " " ^ data_string ^ " >> " ^ res_file);
    (*done;*)
    read_results chan;
    close_in chan

  (* method get_cycles = string_of_int (total_cycle/data_nb) *)
  (* method get_instructions = string_of_int (total_instruction/data_nb) *)
  (* method get_floatinstr = string_of_int (fp_instruction/data_nb) *)
  (* method get_ipc = String.sub (string_of_float  *)
  (*    (float_of_int (total_instruction/nb) /. float_of_int(total_cycle/nb))) 0 4 *)

  method get_cycles = string_of_int total_cycle
  method get_instructions = string_of_int total_instruction
  method get_floatinstr = string_of_int fp_instruction
  method get_ipc = try String.sub (string_of_float 
     (float_of_int (total_instruction) /. float_of_int(total_cycle))) 0 4 with | Invalid_argument _ -> "-1"


  (** String representation of the data *)
  (* method to_string () = *)
  (*   "run: " ^ (string_of_int data_nb) ^ " times...\n" ^ *)
  (*     "\tCycles : "       ^ string_of_int (total_cycle      /data_nb) ^ "\n" ^ *)
  (*     "\tInstructions : " ^ string_of_int (total_instruction/data_nb) ^ "\n" ^ *)
  (*     "\tFP Instr. : "    ^ string_of_int (fp_instruction   /data_nb) ^ "\n" ^ *)
  (*     "\tIPC : "          ^ string_of_float  *)
  (*        (float_of_int (total_instruction/nb) /. float_of_int(total_cycle/nb)) *)
  method to_string () =
      "\tCycles : "       ^ string_of_int total_cycle       ^ "\n" ^
      "\tInstructions : " ^ string_of_int total_instruction ^ "\n" ^
      "\tFP Instr. : "    ^ string_of_int fp_instruction    ^ "\n" ^
      "\tIPC : "          ^ string_of_float 
         (float_of_int (total_instruction) /. float_of_int(total_cycle))


end


(******************************************************************************)

(** Error calculus *)

(** Compute an absolute error 
    @param value True value
    @param measure Measured value *)
let absolute_error value measure =
  value -. measure

(** Compute a relative error 
    @param value True value
    @param measure Measured value *)
let relative_error value measure =
  if value <> 0.0 
  then 
    abs_float (absolute_error value measure) /. abs_float value
  else 
    raise MyDivision_by_zero

(** Compute the number of common digits with a relative error
    @param rel_err A relative error *)
let common_digits rel_err =
  let log2 x = (log x) /. (log 2.0) in
  match rel_err with
  | 0.0 -> Config.default_float_prec
  | x   ->
    if x > 1.0 then
      0
    else
      int_of_float (floor (-.(log2 x)))



(******************************************************************************)

(** Data lists manipulation *)

(** Output a file with the accuracy results
    @param file Output file
    @param data Data to write in file *)
let output_accuracy_results file data =
  let chan = open_out file in
  List.iter (
    fun i ->
      output_string chan (i#accuracy_to_string () ^ "\n")
  ) data;
  close_out chan


(** Output a file with the performances results
    @param file Output file
    @param ori Performances of the original program
    @param hd Performances of the high definition program *)
let output_performances_results file ori hd =
  let chan = open_out file in
  output_string chan 
                   "program \"Original Program\" \"Double-Double Program\"\n";
  output_string chan ("\"#Cycles\" " ^ 
                                   ori#get_cycles ^ " " ^ hd#get_cycles ^ "\n");
  output_string chan ("\"#Instructions\" " ^ 
                      ori#get_instructions  ^ " " ^ hd#get_instructions ^ "\n");
  output_string chan ("\"#FP Instr.\" " ^ 
                          ori#get_floatinstr  ^ " " ^ hd#get_floatinstr ^ "\n");
  output_string chan     ("\"IPC\" " ^ ori#get_ipc  ^ " " ^ hd#get_ipc ^ "\n" );
  close_out chan

(** Output a file with the performances results
    @param file Output file
    @param ori Performances of the original program
    @param hd Performances of the high definition program
    @param dd Performanes of the Double-Double program *)
let output_performances_results2 file ori hd dd =
  let chan = open_out file in
  output_string chan "program \"source\" \"cible\" \
                                                  \"\\\\134textit\\\\{double-double\\\\}\"\n";
  output_string chan ("\"\\\\134# cycles\" " ^ 
             ori#get_cycles ^ " " ^ hd#get_cycles ^ " " ^ dd#get_cycles ^ "\n");
  output_string chan ("\"\\\\134# instructions\" " ^ ori#get_instructions  ^ " " ^ 
                        hd#get_instructions ^ " " ^ dd#get_instructions ^ "\n");
  output_string chan ("\"\\\\134# instr. flottantes\" " ^ ori#get_floatinstr  ^ " " ^ 
                            hd#get_floatinstr ^ " " ^ dd#get_floatinstr ^ "\n");
  output_string chan 
      ("\"\\\\134textsc\\\\{ipc\\\\}\" " ^ ori#get_ipc  ^ " " ^ hd#get_ipc ^ " " ^ dd#get_ipc ^ "\n" );
  close_out chan


(** Output files with the relative errors and the number of common digits
    @param file_err Output relative errors file
    @param file_dig Output common digits number file
    @param ori Data of the original program
    @param hd Data of the high definition program 
    @return Mean of relative errors and mean of common digits arrays *)
let output_errors file_err file_dig ori hd =
  let chan_err = open_out file_err in
  let chan_dig = open_out file_dig in
  let size = List.length (Str.split (Str.regexp " ") 
                                     ((List.hd ori)#accuracy_to_string ())) in
  let moy_err = Array.make (size) ([]) in
  let moy_dig = Array.make (size) ([]) in
  List.iter2 (
    fun i j -> 
      let ac_ori = (Str.split (Str.regexp " ") (i#accuracy_to_string ())) in
      let ac_hd  = (Str.split (Str.regexp " ") (j#accuracy_to_string ())) in
      let id = ref 0 in
      List.iter2 (
        fun m n ->
	  let value   = float_of_string n in
	  let measure = float_of_string m in
	  let rel_err = 
              try relative_error value measure with MyDivision_by_zero -> -.1. in
	  let digits = if rel_err = -.1. then -1 else common_digits rel_err in
          if rel_err <> -.1. then moy_err.(!id) <- moy_err.(!id) @ [(rel_err)];
          if digits  <> -1   then moy_dig.(!id) <- moy_dig.(!id) @ [(digits)];
          (if rel_err <> -.1. then fprintf chan_err "%.20e " rel_err else fprintf chan_err "x ");
          (if digits <> -1 then fprintf chan_dig "%d " digits else fprintf chan_dig "x ");
          id := !id + 1
      ) ac_ori ac_hd;
      output_string chan_err "\n";
      output_string chan_dig "\n";
  ) ori hd;
  close_out chan_err;
  close_out chan_dig;

  let rec sum lst acc = (***TODO CODER CETTE FONCTION AILLEURS ET PLUS INTELLIGEMMENT*)
    match lst with
    | [] -> acc
    | h::t -> sum t (acc+h)
  in
  let rec float_sum lst acc = (***TODO CODER CETTE FONCTION AILLEURS ET PLUS INTELLIGEMMENT*)
    match lst with
    | [] -> acc
    | h::t -> float_sum t (acc+.h)
  in

  let cmp x y = if x = y then 0 else (if x > y then 1 else -1) in
  let res_moy_err = Array.map (
    fun i -> try
     (float_sum (List.sort cmp i) 0.0) /. float_of_int (List.length i) 
     with Division_by_zero -> 0.0
  ) moy_err in
  let res_moy_dig = Array.map (
    fun i -> try
    (sum i 0) / (List.length i)
    with Division_by_zero -> 0
  ) moy_dig in
  (res_moy_err, res_moy_dig)



let compute_errors ori hd =
  let size = List.length (Str.split (Str.regexp " ") 
                                     ((List.hd ori)#accuracy_to_string ())) in
  let moy_err = Array.make (size) ([]) in
  let moy_dig = Array.make (size) ([]) in
  List.iter2 (
    fun i j -> 
      let ac_ori = (Str.split (Str.regexp " ") (i#accuracy_to_string ())) in
      let ac_hd  = (Str.split (Str.regexp " ") (j#accuracy_to_string ())) in
      let id = ref 0 in
      List.iter2 (
        fun m n ->
	  let value   = float_of_string n in
	  let measure = float_of_string m in
	  let rel_err = 
              try relative_error value measure with MyDivision_by_zero -> -.1. in
	  let digits = if rel_err = -.1. then -1 else common_digits rel_err in
          if rel_err <> -.1. then moy_err.(!id) <- moy_err.(!id) @ [(rel_err)];
          if digits  <> -1   then moy_dig.(!id) <- moy_dig.(!id) @ [(digits)];
       (*   (if rel_err <> -.1. then fprintf stdout "%.20e " rel_err else fprintf stdout "x ");
          (if digits <> -1 then fprintf stdout "%d " digits else fprintf stdout "x ");*)
          id := !id + 1
      ) ac_ori ac_hd;
    (*  output_string stdout "\n";
      output_string stdout "\n";*)
  ) ori hd;

  let rec sum lst acc = (***TODO CODER CETTE FONCTION AILLEURS ET PLUS INTELLIGEMMENT*)
    match lst with
    | [] -> acc
    | h::t -> sum t (acc+h)
  in
  let rec float_sum lst acc = (***TODO CODER CETTE FONCTION AILLEURS ET PLUS INTELLIGEMMENT*)
    match lst with
    | [] -> acc
    | h::t -> float_sum t (acc+.h)
  in

  let cmp x y = if x = y then 0 else (if x > y then 1 else -1) in
  let res_moy_err = Array.map (
    fun i -> try
     (float_sum (List.sort cmp i) 0.0) /. float_of_int (List.length i) 
     with Division_by_zero -> 0.0
  ) moy_err in
  let res_moy_dig = Array.map (
    fun i -> try
    (sum i 0) / (List.length i)
    with Division_by_zero -> 0
  ) moy_dig in
  (res_moy_err, res_moy_dig)
