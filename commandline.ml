(******************************************************************************)

(** Scan the command line and set some program variables. 
    Use the Arg module of ocaml *)

open Config

(******************************************************************************)

(** Text printed if there is something wrong with the command arguments. *)
let usage_text = 
  "usage :\tsynthd [options] files...\n" ^
  "\nSynth-HD version : " ^ Config.version ^ "\n\n"


let files:string list ref = ref []         (** C files list from command line *)
let data_file = ref ""                      (** Data file (one data per line) *)

let out_file = ref ""                                  (** Result C file name *)

let prospect = ref false                         (** Run the prospection pass *)
let verbose = ref false

let cohd_args = ref ""                                (** Arguments for Co-HD *)
let manual_mode = ref false
let devel_mode = ref false

let code_synthesis = ref false
let code_spec = ref ""

let compiler = ref "gcc"                                    (** Compiler name *)
let comp_opt = ref ""                                    (** Compiler options *)
let args_is_set = ref false                      (** Compiler options are set *)

(******************************************************************************)

(** Scan the options from the command line and set variables. *)
let scan_options = Arg.align
[
  ("-pp"  , Arg.Set prospect          , " Do a prospection pass");
  ("-man" , Arg.String (fun x -> cohd_args := x; manual_mode := true)
            , "<\"cohd_args_list\"> Specify arguments for a manual use of Co-HD");
  ("-sy"  , Arg.String (fun x -> code_spec := x; code_synthesis := true)
            , "<spec> Do synth0 code synthesis with <spec> tradeoff");
  ("-dev" , Arg.Set devel_mode        , " Do devel mode pass");
  ("-n"   , Arg.Set_int Config.nb_iter, 
                            "<nb> number of iteration to measure performances");
  ("-o"   , Arg.Set_string out_file   , "<output> Output to the <output> file");
  ("-dat" , Arg.Set_string data_file  , "<file> Data file <file>");
  ("-v"   , Arg.Set verbose           , " Be verbose");
  ("-c"   , Arg.Set_string compiler   , "<compiler> Specify the compiler");
  ("-copt", Arg.String (fun x -> comp_opt := x; args_is_set := true)
                            , "<\"args_list\"> Specify the compiler arguments");

  ("-alpha", Arg.Set_int Config.alpha, 
                            "<nb> ... alpha coef ...");
  ("-beta" , Arg.Set_int Config.beta, 
                            "<nb> ... beta coef ...");
  ("-ddbe" , Arg.Set_int Config.ddbe, 
                            "<nb> ... ddbe ...");
  ("-nu" , Arg.Set_int Config.nu, 
                            "<nb> ... nu ...");
  ("-t2limit" , Arg.Set_int Config.t2limit, 
                            "<nb> ... t2limit ...");

]


(** Scan the command line. *)
let scan_cmd_line =
  Arg.parse scan_options 
    (fun file -> files := file :: !files) usage_text;
  if(!files = []) then 
    begin
      Arg.usage scan_options usage_text;
      exit 2;
    end
