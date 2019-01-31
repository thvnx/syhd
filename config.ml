(******************************************************************************)

(** Configuration of Synth-HD. *)


(******************************************************************************)

(** Miscellaneous settings. *)

let version = "1.0"                             (** Version number of Synth-HD*)

let nb_iter = ref 100000     (** Number of iteration to measures performances *)
let default_float_prec = 53       (** Precision bits of double floating point *)

(******************************************************************************)

(** Paths, files, executables, or tools names settings. *)

                                                 (** Path to Co-HD executable *)
let cohd_path     = "/home/ltheveno/thesis/devel/bin/cohd"
(* let cohd_path     = "/home/laurent/thesis/devel/bin/cohd"*)
let tmp_cohd_file = ".out.c"               (** Temporary output file of Co-HD *)
let tmp_file      = ".out.dat"              (** Output data file for Synth-HD *)
let tmp_plot      = "plot"                 (** Temporaty gnuplot script file *)
let tmp_exe       = "./a.out"                   (** Temporary executable name *)
let gcc_options   = "-O2 -lpapi -mfpmath=sse -msse4.2"    (** options for gcc *)




let alpha = ref 1
let beta = ref 1
let ddbe = ref 53
let nu = ref 10
let t2limit = ref 4
