(******************************************************************************)

(** Plot Library *)

module PLT = Gnuplot
module DAT = Datamanager
module CMD = Commandline
module SYS = Syscalls


(******************************************************************************)

(** Output the results in a plot 
    @param name Prefix name of the output files
    @param or_prec Accuracy results from the original program
    @param or_perf Performances results from the original program
    @param dd_prec Accuracy results from the double-precision overloaded program
    @param dd_perf Performances results from the double-precision overloaded
    @param numerical True if the data is one numerical value *)
let plot2 name or_prec or_perf dd_prec dd_perf numerical =
                                               (** Print the results in files *)
  let or_prec_file = (name ^ "_or_prec.dat") in
  let dd_prec_file = (name ^ "_hd_prec.dat") in
  DAT.output_accuracy_results or_prec_file or_prec;
  DAT.output_accuracy_results dd_prec_file dd_prec;
  let perf_file = (name ^ "_perf.dat") in
  DAT.output_performances_results perf_file or_perf dd_perf;
  let rel_err_file = (name ^ "_relerr.dat") in
  let digits_file = (name ^ "_prec_digits.dat") in
  let (moy_err, moy_dig) = 
                   DAT.output_errors rel_err_file digits_file or_prec dd_prec in

  let chan = open_out Config.tmp_plot in                 (** Build the script *)
  PLT.header chan "1024,768" (name);
  output_string chan "set multiplot layout 2,2\n";
  output_string chan "set xtics rotate by -30 offset 0,-1\n";

  (* First graph *)
  output_string chan "set title \"Accuracy\"\n";
  output_string chan "set y2tics\n";
  let titles = (List.nth or_prec 0)#get_accuracy_titles () in
  let plot1 = ref "" in
  let id = ref 2 in
  List.iter (
    fun i -> 
      plot1 := !plot1 ^ (
      if numerical then 
        "'<paste " ^ !CMD.data_file ^ " " ^ or_prec_file ^ "' u 1:" ^ string_of_int !id
      else "'" ^ or_prec_file ^ "' u " ^ string_of_int (!id-1)
      ) ^ " w lines t \"" ^ i ^ 
              " (original program : left axis)\", " ^ (
      if numerical then
        "'<paste " ^ !CMD.data_file ^ " " ^ dd_prec_file ^ "' u 1:" ^ string_of_int !id 
      else "'" ^ dd_prec_file ^ "' u " ^ string_of_int (!id-1)
      ) ^ " axes x1y2 w lines t \"" ^ i ^ 
              " (double-double program : right axis)\", ";
      id := !id + 1
  ) titles;
  output_string chan ("plot " ^ 
                       (String.sub !plot1 0 ((String.length !plot1)-2)) ^ "\n");
  output_string chan "unset y2tics\n";

  (* Second graph *)
  output_string chan "set title \"Performance Counts\"\n";
  output_string chan "set style data histogram\nset style fill solid\n";

  output_string chan ("plot '" ^ perf_file ^ "' u 2:xtic(1) t columnheader(2),\
               '' using 0:2:2 with labels offset -13,2 rotate by 75 notitle," ^
                      "'" ^ perf_file ^ "' u 3:xtic(1) t columnheader(3), \
              '' using 0:3:3 with labels offset -9,2 rotate by 75 notitle\n") ;
  output_string chan "unset style\n";

  (* Third graph *)
  output_string chan "set title \"Relative Error (logarithm scale)\"\n";
  output_string chan "set logscale y\n";
  let plot3 = ref "" in
  let id = ref 2 in
  List.iter (
    fun i -> 
      plot3 := !plot3 ^ (
      if numerical then
      "'<paste " ^ !CMD.data_file ^ " " ^ rel_err_file ^ "' u 1:" ^ 
              string_of_int !id 
      else "'" ^ rel_err_file ^ "' u " ^ string_of_int (!id-1)
      ) ^ " w lines t \"" ^ i ^ "\", \
              " ^ string_of_float (moy_err.(!id-2)) ^ " t \"" ^ i ^ " (mean)\", ";
      id := !id + 1
  ) titles;
  output_string chan ("plot " ^ 
                       (String.sub !plot3 0 ((String.length !plot3)-2)) ^ "\n");

  output_string chan "unset logscale y\n";

 (* Fourth graph *)
  output_string chan "set title \"Number of Correct Precision Bits\"\n";
  let plot4 = ref "" in
  let id = ref 2 in
  List.iter (
    fun i -> 
      plot4 := !plot4 ^ (
      if numerical then
      "'<paste " ^ !CMD.data_file ^ " " ^ digits_file ^ "' u 1:" ^ 
              string_of_int !id 
      else "'" ^ digits_file ^ "' u " ^ string_of_int (!id-1)
      ) ^ " w linespoints t \"" ^ i ^ "\", \
              " ^ string_of_int (moy_dig.(0)) ^ " t \"" ^ i ^ " (mean)\", ";
      id := !id + 1
  ) titles;
  output_string chan ("plot " ^ 
                       (String.sub !plot4 0 ((String.length !plot4)-2)) ^ "\n");

  output_string chan "unset multiplot\n";
  close_out chan;
  let cmd = "gnuplot " ^ Config.tmp_plot in                (** Launch GNUPLOT *)
  SYS.command cmd;
  Sys.remove or_prec_file;
  Sys.remove dd_prec_file;
  Sys.remove perf_file;
  Sys.remove rel_err_file;
  Sys.remove digits_file


(** Same as plot2 but with a third program : original - cohd - double-double*)
let plot3 name or_prec or_perf hd_prec hd_perf dd_prec dd_perf numerical =
                                               (** Print the results in files *)
  let or_prec_file = ((*name ^ *)"or_prec.dat") in
  let dd_prec_file = ("dd_prec.dat") in
  let hd_prec_file = ("hd_prec.dat") in
  DAT.output_accuracy_results or_prec_file or_prec;
  DAT.output_accuracy_results dd_prec_file dd_prec;
  DAT.output_accuracy_results hd_prec_file hd_prec;
  let perf_file = ("perf.dat") in
  DAT.output_performances_results2 perf_file or_perf hd_perf dd_perf;
  let or_rel_err_file = ("or_relerr.dat") in
  let or_digits_file = ("or_prec_digits.dat") in
  let hd_rel_err_file = ("hd_relerr.dat") in
  let hd_digits_file = ("hd_prec_digits.dat") in
  let (moy_err_or, moy_dig_or) = 
             DAT.output_errors or_rel_err_file or_digits_file or_prec dd_prec in
  let (moy_err_hd, moy_dig_hd) = 
             DAT.output_errors hd_rel_err_file hd_digits_file hd_prec dd_prec in


  let chan = open_out Config.tmp_plot in                 (** Build the script *)
  PLT.header chan "1024,768" (name);
  output_string chan "set multiplot layout 2,2\n";
  output_string chan "set xtics rotate by -30 offset 0,-1\n";

  (* First graph *)
  output_string chan "set title \"Accuracy\"\n";
  output_string chan "set y2tics\n";
  let titles = (List.nth or_prec 0)#get_accuracy_titles () in
  let plot1 = ref "" in
  let id = ref 2 in
  List.iter (
    fun i -> 
      plot1 := !plot1 ^ (
      if numerical then
      "'<paste " ^ !CMD.data_file ^ " " ^ or_prec_file ^ "' u 1:" ^ 
              string_of_int !id ^ " w lines t \"" ^ i ^ 
              " (original program : left axis)\", " ^
      "'<paste " ^ !CMD.data_file ^ " " ^ hd_prec_file ^ "' u 1:" ^ 
              string_of_int !id ^ " w lines t \"" ^ i ^ 
              " (high definition program : left axis)\", " ^
      "'<paste " ^ !CMD.data_file ^ " " ^ dd_prec_file ^ "' u 1:" ^ 
              string_of_int !id ^ " axes x1y2 w lines t \"" ^ i ^ 
              " (double-double program : right axis)\", "
      else 
      "'" ^ or_prec_file ^ "' u " ^ 
              string_of_int (!id-1) ^ " w lines t \"" ^ i ^ 
              " (original program : left axis)\", " ^
      "'" ^ hd_prec_file ^ "' u " ^ 
              string_of_int (!id-1) ^ " w lines t \"" ^ i ^ 
              " (high definition program : left axis)\", " ^
      "'" ^ dd_prec_file ^ "' u " ^ 
              string_of_int (!id-1) ^ " axes x1y2 w lines t \"" ^ i ^ 
              " (double-double program : right axis)\", "
      );
      id := !id + 1
  ) titles;
  output_string chan ("plot " ^ 
                       (String.sub !plot1 0 ((String.length !plot1)-2)) ^ "\n");
  output_string chan "unset y2tics\n";

  (* Second graph *)
  output_string chan "set title \"Performance Counts\"\n";
  output_string chan "set style data histogram\nset style fill solid\n";

  output_string chan ("plot '" ^ perf_file ^ "' u 2:xtic(1) t columnheader(2),\
               '' using 0:2:2 with labels offset -14,2 rotate by 75 notitle," ^
                      "'" ^ perf_file ^ "' u 3:xtic(1) t columnheader(3), \
              '' using 0:3:3 with labels offset -11,2 rotate by 75 notitle," ^
                      "'" ^ perf_file ^ "' u 4:xtic(1) t columnheader(4), \
              '' using 0:4:4 with labels offset -8,2 rotate by 75 notitle\n");
  output_string chan "unset style\n";

  (* Third graph *)
  output_string chan "set title \"Relative Error (logarithm scale)\"\n";
  output_string chan "set logscale y\n";
  let plot3 = ref "" in
  let id = ref 2 in
  List.iter (
    fun i -> 
      plot3 := !plot3 ^ (
      if numerical then
      "'<paste " ^ !CMD.data_file ^ " " ^ or_rel_err_file ^ "' u 1:" ^ 
              string_of_int !id ^ " w lines t \"" ^ i ^ " (original program)\", "
              ^ string_of_float (moy_err_or.(!id-2)) ^ " t \"" ^ i ^ 
              " (mean - original program)\", " ^
      "'<paste " ^ !CMD.data_file ^ " " ^ hd_rel_err_file ^ "' u 1:" ^ 
              string_of_int !id ^ " w lines t \"" ^ i ^ " (high definition \
              program)\", " ^ string_of_float (moy_err_hd.(!id-2)) ^ " t \"" ^ i
              ^ " (mean - high definition program)\", "
      else 
      "'" ^ or_rel_err_file ^ "' u " ^ 
              string_of_int (!id-1) ^ " w lines t \"" ^ i ^ " (original program)\", "
              ^ string_of_float (moy_err_or.(!id-2)) ^ " t \"" ^ i ^ 
              " (mean - original program)\", " ^
      "'" ^ hd_rel_err_file ^ "' u " ^ 
              string_of_int (!id-1) ^ " w lines t \"" ^ i ^ " (high definition \
              program)\", " ^ string_of_float (moy_err_hd.(!id-2)) ^ " t \"" ^ i
              ^ " (mean - high definition program)\", "
      );
      id := !id + 1
  ) titles;
  output_string chan ("plot " ^ 
                       (String.sub !plot3 0 ((String.length !plot3)-2)) ^ "\n");

  output_string chan "unset logscale y\n";

 (* Fourth graph *)
  output_string chan "set title \"Number of Correct Precision Bits\"\n";
  let plot4 = ref "" in
  let id = ref 2 in
  List.iter (
    fun i -> 
      plot4 := !plot4 ^ (
      if numerical then
      "'<paste " ^ !CMD.data_file ^ " " ^ or_digits_file ^ "' u 1:" ^ 
              string_of_int !id ^ " w linespoints t \"" ^ i ^ 
              " (original program)\", " ^ string_of_int (moy_dig_or.(0)) ^ 
              " t \"" ^ i ^ " (mean - original program)\", " ^
      "'<paste " ^ !CMD.data_file ^ " " ^ hd_digits_file ^ "' u 1:" ^ 
              string_of_int !id ^ " w linespoints t \"" ^ i ^ 
              " (high definition program)\", " ^ string_of_int (moy_dig_hd.(0))
              ^ " t \"" ^ i ^ " (mean - high definition program)\", "
      else
      "'" ^ or_digits_file ^ "' u " ^ 
              string_of_int (!id-1) ^ " w linespoints t \"" ^ i ^ 
              " (original program)\", " ^ string_of_int (moy_dig_or.(0)) ^ 
              " t \"" ^ i ^ " (mean - original program)\", " ^
      "'" ^ hd_digits_file ^ "' u " ^ 
              string_of_int (!id-1) ^ " w linespoints t \"" ^ i ^ 
              " (high definition program)\", " ^ string_of_int (moy_dig_hd.(0))
              ^ " t \"" ^ i ^ " (mean - high definition program)\", "
      );
      id := !id + 1
  ) titles;
  output_string chan ("plot " ^ 
                       (String.sub !plot4 0 ((String.length !plot4)-2)) ^ "\n");

  output_string chan "unset multiplot\n";
  close_out chan;
  let cmd = "gnuplot " ^ Config.tmp_plot in                (** Launch GNUPLOT *)
  SYS.command cmd
(*;  Sys.remove or_prec_file;
  Sys.remove hd_prec_file;
  Sys.remove dd_prec_file;
  Sys.remove perf_file;
  Sys.remove or_rel_err_file;
  Sys.remove or_digits_file;
  Sys.remove hd_rel_err_file;
  Sys.remove hd_digits_file
*)
