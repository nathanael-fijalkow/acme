open Structure
open Automata
open Monoid

val create_dotty_file_aut : automaton -> out_channel -> unit
val create_dotty_file_mon : automaton -> monoid -> out_channel -> unit
