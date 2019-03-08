(** [clientmain.ml] handles the code for the player's interaction with the 
    server. *)
open Unix
open ANSITerminal


(* Note: [acc] is [Unix.accept s_descr] if the server is sending the string,
    and is [(s, (Unix.getpeername s))] if the client is sending the string *)
(** [send_str msg acc] sends the string [msg] to the socket of address [acc] *)
let send_str msg acc =
  let _ = Unix.sendto_substring (fst acc) msg 0 (String.length msg) [] (snd acc)
  in ()

(** [while_client_func ip port s] is the while loop that runs during the game,
    communicating between the server with [ip] and [port] and the player with 
    socket [s]. *)
let while_client_func ip port s = 
  let data_read = Bytes.create 1000 in
  let data_length = Unix.recv s data_read 0 1000 [] in
  if (data_length <> 0) then
    let info = (String.sub (Bytes.to_string data_read) 0 data_length) in
    (* We want to know the status of the game: for example, if it's the current
       client's turn because if so we need to prompt input; if the game ends;
       if the client's attempted move is invalid, etc. *)
    (* Case when [info] is "$ What would you like to do next?". *)
    begin
      if ((String.contains info '$')) 
      then (
        Pervasives.print_endline info;
        match read_line () with
        | exception End_of_file -> ()
        | (input : string) ->
          let _ = send_str input (s, getpeername s) in ();
          if (input = "quit") then exit 0 else ())
      (* winner exists, and game ends *)
      else if (String.contains info '*')
      then (let _ = send_str "quit" (s, getpeername s) in ();
            Pervasives.print_endline info; exit 0)
      (* terminating the game is indicated by the server with char '%' *)
      else if (String.contains info '%') then (
        print_endline "Game interrupted. "; exit 0
      ) 
      else Pervasives.print_endline info
    end
  else ()

(** [client_func ip port] is the communication between the server with [ip] and
    [port] and the local client. *)
let client_func ip port =
  try
    let s = socket PF_INET SOCK_STREAM 0 in
    print_endline "Pending Connection...\n";
    connect s (ADDR_INET ((inet_addr_of_string ip), port));
    print_endline "Connected!\n";
    (while true do
       while_client_func ip port s
     done)
  with (Failure _) -> exit 0

(** [getPort () ip] prompts the client for the port number of the server they
    are trying to connect to. *)
let rec getPort ip =
  print_string [white] "Please enter port number. \n";
  try
    print_newline ();
    Pervasives.print_endline "> ";
    let input = read_line () in 
    match input with 
    | exception End_of_file -> ()
    | a when (String.lowercase_ascii a) = "quit" -> exit 0
    | a -> client_func ip (Parse.readPort a)
  with (Parse.InvalidCommand) -> 
    print_string [Bold; red] "I'm sorry, that is not a valid port number.";
    print_newline ();
    getPort ip

(** [main ()] is the main system for the client end, prompts for the IP address
    of the server the client is trying to connect to. *)
let rec main () = 
  ANSITerminal.print_string [Bold; red] 
    "\n\nWelcome to Uno!\n";
  try
    print_endline "Please enter the ip address! \n";
    Pervasives.print_string  "> ";
    let temp = read_line () in
    match temp with
    | exception End_of_file -> ()
    | a when (String.lowercase_ascii a) = "quit" -> exit 0
    | a when (a = "1") -> raise (Failure "Can't be 1")
    | n -> getPort (Parse.readIp n)
  with (Failure _) -> 
    print_string ([red])
      "Invalid IP address.";
    print_newline(); main ()

(** Run the client main. *)
let () = main ()