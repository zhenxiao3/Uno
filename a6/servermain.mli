(** [play_game t s] is the function that initiates a online game defined by
    the initial state [t], and hosted by server socket [s] *)
val play_game: State.t -> Unix.file_descr -> unit