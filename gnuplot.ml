(******************************************************************************)

(** GNUPLOT script construction functions *)


(******************************************************************************)

(** GNUPLOT common *)

(** Output header 
    @param chan Output channel
    @param size Plot size
    @param name Plot name *)
let header chan size name =
  output_string chan ("set terminal svg enhanced size " ^ size ^ " fsize 10\n");
  output_string chan ("set output '" ^ name ^ ".svg'\n");
