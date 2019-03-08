open ANSITerminal
open Unix
open State

(** [sock_drawer server_sock acc lst] is the list of players assigned with
    incoming socket addresses. *)
let rec sock_drawer server_sock acc = function
  | [] -> acc
  | h :: t -> let a = (accept server_sock) in
    sock_drawer server_sock ({h with sock = a} :: acc) t

(** [tryChooseEnv ()] prompts the user for the environment of the game and 
    runs the game if an appropriate environment is selected and clients connect
    properly. 
    Raises: [Parse.InvalidCommand] if the user inputs and incorrect IP 
    address, port, or environment type. *)
let tryChooseEnv () = 
  print_newline ();
  Pervasives.print_string "> ";
  let input = read_line () in 
  match input with 
  | exception End_of_file -> ()
  | a when (String.lowercase_ascii a) = "quit" -> exit 0
  | a -> begin let env = Parse.readEnv a in
      if (env = "offline") then Offlinemain.main () 
      else 
        (* TODO: make online
           try catch connection *)
        let s_descr = (socket PF_INET SOCK_STREAM 0) in
        let ip = "127.0.0.1" in
        let port = 8888 in
        bind s_descr (ADDR_INET((inet_addr_of_string ip), port));
        listen s_descr 2;
        print_endline 
          ("The IP of this server is " ^ ip ^
           ", and the port of this server is "
           ^ (string_of_int port) ^ ".\n");
        print_endline "Pending Connection...\n";
        let temp = (State.init_game 2 0 0) in
        let init_state = 
          {temp with players = List.rev (sock_drawer s_descr [] temp.players)}
        in
        Pervasives.print_endline "Connected!\n";
        Servermain.play_game init_state s_descr
    end 

(** [chooseEnv ()] prompts the Internet environment of the game and runs the
    game system according to user input *)
let rec chooseEnv () =
  print_string [white] "What environment would you like to play in? 
  Choices are 'Online' and 'Offline'.";
  try
    tryChooseEnv ()
  with (Parse.InvalidCommand) -> 
    print_string [Bold; red] 
      "Invalid environment. Valid environments include 'Online' and 'Offline'.";
    print_newline ();
    chooseEnv ()

(**[ main ()] starts the game engine. *)
let rec main () = 
  ANSITerminal.print_string [Bold; red] 
    ("\n\n" ^ (Parse.displayCard "UNO.txt") ^ "\n"); 
  try
    chooseEnv ()
  with (Failure _) -> 
    print_string ([red])
      "Invalid environment. Valid environments include 'Online' and 'Offline'.";
    print_newline(); main ()

(** Run the game engine. *)
let () = main ()