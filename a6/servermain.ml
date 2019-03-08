open Deck
open ANSITerminal
open State
open Unix

(** [custom_mod x y] is the "mod" operation that handles negative [x]'s. *)
let rec custom_mod (x : int) (y : int) = 
  if (x > 0) then (x mod y) else (custom_mod (x + y) y)

(** [send msg acc] sends the string [msg] to the socket with address [acc] *)
let send (msg : string) (acc : (file_descr * sockaddr)) = 
  try
    ignore
      (let _ = (sendto_substring (fst acc) msg 0 (String.length msg) []
                  (snd acc)) in ())
  with (Unix.Unix_error (e, _, _)) -> Printf.eprintf "Can't send: %s\n%!"
                                        (Unix.error_message e);
    exit 1

(** [masssend msg lst] sends the string [msg] to all the sockets with addresses
    in [lst] *)
let masssend (msg : string) (lst : (file_descr * sockaddr) list) =
  let _ = send msg (List.hd lst) in
  let _ = send msg (List.nth lst 1) in ()

(** [receive_str s maxlen] is the string received from the socket of address
    [s] that has maximum length of [maxlen] *)
let receive_str s maxlen = 
  let data_read = Bytes.create maxlen in
  let data_length = Unix.recv s data_read 0 maxlen [] in
  String.sub (Bytes.to_string data_read) 0 data_length

(** [color2string c] is the color of the card represented as a string. If the 
    card is a wild card, the color is Wild. *)
let color2string (c : Deck.color) = 
  match c with 
  | Green -> "Green "
  | Red -> "Red "
  | Blue -> "Blue "
  | Yellow -> "Yellow "
  | Wild -> "Wild"

(** [cardnum2string i] is the card number or action represented as a string. 
    Example: [cardnum2string 9] is ["9"], [cardnum2string 12] is ["Reverse"]. *)
let cardnum2string  = function
  | x when x < 10 -> string_of_int x
  | 10 -> "Skip"
  | 11 -> "PlusTwo"
  | 12 -> "Reverse"
  | _ -> failwith "Not a valid card number so not possible"

(** [print_hand_helper acc lst] is the string of card names in hand [lst] *)
let rec print_hand_helper acc = function
  | [] -> acc
  | {color = x; value = y; card_type = z}::t -> 
    begin match y with 
      | -1 -> print_hand_helper (acc ^ "Wild, ") t
      | -2 -> print_hand_helper (acc ^ "Wild PlusFour, ") t
      | _ -> let str = (color2string x)^(cardnum2string y) in
        print_hand_helper (acc ^ str ^ ( ", ")) t
    end 

(** [print_hand s lst] accumulates the cards in the current players hand into a
    string and sends this information to the client with socket address [s]. *)
let print_hand s lst =
  let _ = send ((print_hand_helper "" lst)) s in ()

(** [lastplayed_card discPile] is the most recent card played in the game. 
    In other words, it is the top card of discard pile. *)
let lastplayed_card = function
  | [] -> ""
  | {color = x; value = y; card_type = z}::t -> 
    begin match y with
      | -1 -> (color2string x) ^ "Wild"
      | -2 -> (color2string x) ^ "PlusFour"
      | _ -> (color2string x) ^ (cardnum2string y)
    end 

(** [cardname card] is the display name of [card] as a string. *)
let cardname = function 
  | {color = x; value = y; card_type = z} -> 
    begin match y with
      | -1 -> (color2string x) ^ "Wild" 
      | -2 -> (color2string x) ^ "PlusFour"
      |  _ -> (color2string x) ^ (cardnum2string y)
    end 

(** [playhelperPlusFour game_state x s] is the state of the game after the
    player plays a plusfour. *)
let playhelperPlusFour game_state x s =
  let socks = List.map (fun x -> x.sock) game_state.players in
  let cur_player = List.nth game_state.players game_state.turn in
  let _ = send ("You've chosen to play a " ^ (cardname x) ^ "\n") s in 
  begin match 
      (State.play (State.change_wildcard_color game_state 
                     {color = Wild; value = -2; card_type = "PlusFour"} 
                     (x.color)) x) with
  | exception State.InvalidMove -> 
    let _ = send "Sorry that move is Invalid. Try again.\n" s in game_state
  | a ->  let next_player = List.nth game_state.players 
              ((a.turn + 1)  mod (List.length game_state.players)) in 
    let _ = masssend ((cur_player.name) ^ " played: " ^ ((cardname x)^"\n"))
        socks in
    let _ = masssend ("Color changed to " ^(color2string x.color) ^ ". \n" ^
                      (next_player.name)^ " has drawn four cards.\n") socks in 
    a
  end

(** [playhelperWild game_state x s] is the state of the game after the player
    plays a Wild. *)
let playhelperWild game_state x s =
  let cur_player = List.nth game_state.players game_state.turn in
  let _ = send "You've chosen to play a " s in 
  let socks = List.map (fun x -> x.sock) game_state.players in
  let _ = send ((cardname x)^"\n") s in 
  begin match
      State.play (State.change_wildcard_color game_state 
                    {color = Wild; value = -1; card_type = "Wild"} 
                    (x.color)) x with 
  | exception State.InvalidMove -> 
    let _ = send "Sorry that move is Invalid. Try again.\n" s in game_state 
  | a -> let _ = masssend ((cur_player.name) ^ " played: " ^
                           ((cardname x)^"\n")) socks in
    let _ = masssend ("Color changed to "^ (color2string x.color)) socks in
    a
  end

(** [playNumCard new_state game_state cur_player socks a] is the game_state 
    after a number card is played. *)
let playNumCard new_state game_state cur_player socks a x =
  let p = (List.nth new_state.players game_state.turn) in
  masssend ("\n" ^ (cur_player.name) ^ " played " ^ (cardname x) ^ "\n") socks;
  let _ = masssend (cur_player.name ^ " has " ^ 
                    (string_of_int (List.length p.hand)) 
                    ^ " cards left.\n") socks in 
  a

(** [playPlusTwo gs prev_player socks new_state cur_player nextplayer a x] 
    is the game_state after a PlusTwo is played. *)
let playPlusTwo gs prev_player socks new_state curplayer nextplayer a x =
  if (gs.mode <> 1 && gs.mode <> 3) 
  then let _ = masssend ((curplayer.name) ^ " played: " ^ 
                         ((cardname x)^"\n")) socks in
    let new_player = List.nth new_state.players gs.turn in
    let op_hand = List.length new_player.hand in
    masssend ("\n" ^ (curplayer.name) ^ " played " ^ (cardname x) ^ "\n") socks;
    let _ = masssend (curplayer.name ^ " has " ^ 
                      (string_of_int op_hand) ^ " cards left.\n") socks in
    masssend 
      (nextplayer.name ^ " has drawn 2 cards and lost their turn.\n") socks; 
    a
  else a  

(** [playSkip cur_player x socks new_state game_state nextplayer a] is the 
    game_state after a Skip is played. *)
let playSkip cur_player x socks new_state game_state nextplayer a =
  let _ = masssend ((cur_player.name) ^ " played: " ^ (cardname x) ^ "\n") 
      socks in
  let new_player = List.nth new_state.players game_state.turn in
  let op_hand = List.length new_player.hand in
  masssend ("\n" ^ (cur_player.name) ^ " played " ^ ((cardname x)^ "\n")) socks;
  let _ = masssend (cur_player.name ^ " has " ^ 
                    (string_of_int (op_hand)) ^ " cards left.\n") socks in
  let _ = masssend (nextplayer.name ^ "'s turn has been skipped.\n") socks in 
  a

(** [playReverse cur_player game_state socks new_state x prev_player a] is the 
    game_state after a Reverse is played. *)
let playReverse cur_player game_state socks new_state x prev_player a =
  let _ = masssend ((cur_player.name) ^ " played: " ^ ((cardname x) ^ "\n")) 
      socks in
  let new_turn = ((List.length new_state.players) - game_state.turn - 1)
                 mod (List.length new_state.players) in
  let new_player = List.nth new_state.players new_turn in
  let op_hand = List.length new_player.hand in
  masssend ("\n" ^ (cur_player.name) ^ " played " ^ (cardname x) ^ "\n") socks;
  let _ = masssend (prev_player.name ^ " has " ^
                    (string_of_int op_hand) ^ " cards left.\n") socks in
  let _ = masssend ("The order has been reversed. It is now " ^ 
                    (List.nth new_state.players new_state.turn).name
                    ^ "'s turn.\n") socks in
  a

(** [playhelperNormal game_state x s] is the state of the game after any card
    that is neither a Wild nor a PlusFour is played. *)
let playhelperNormal game_state x s =
  let socks = List.map (fun x -> x.sock) game_state.players in
  let curplayer = List.nth game_state.players game_state.turn in
  let _ = send ("\nYou chose to play " ^ (cardname x) ^ "\n") s in
  let new_state = State.play game_state x in
  begin match new_state with
    | exception State.InvalidMove -> 
      let _ = send "Sorry that move is Invalid. Try again." s in
      game_state 
    | a -> let nextTurn = (game_state.turn + 1) mod 
                          (List.length game_state.players) in
      let prevTurn = (game_state.turn + 
                      (List.length game_state.players) - 1) mod 
                     (List.length game_state.players) in
      let nextplayer = List.nth (game_state.players) nextTurn in
      let prev_player = List.nth (new_state.players) prevTurn in
      begin match x.value with 
        | 10 -> playSkip curplayer x socks new_state game_state nextplayer a
        | 11 -> playPlusTwo game_state prev_player socks new_state curplayer 
                  nextplayer a x
        | 12 -> playReverse curplayer game_state socks new_state x prev_player a
        | _ -> playNumCard new_state game_state curplayer socks a x
      end
  end

(** [playhelper game_state x s] is the state of the game after a card is played.
    If the move is invalid, it prompts for the user to input another 
    command. *)
let playhelper (game_state : State.t) (x : Deck.card) s = 
  match x.card_type with 
  | "Wild" -> playhelperWild game_state x s
  | "PlusFour" -> playhelperPlusFour game_state x s
  | _ -> playhelperNormal game_state x s

(**[drawprint dec num acc] returns list of the cards that will be drawn from 
   the top of the deck. *)
let rec drawprint (deck: Deck.card list) (num: int) acc = 
  if num = 0 then acc else 
    match deck with 
    | [] -> acc
    | h::t -> h::(drawprint t (num-1) acc)

(** [drawhelper game_state num can_draw sock] is the state after the user draws
    [num] cards. *)
let drawhelper (game_state : State.t) (num : int) (can_draw : bool) sock = 
  if can_draw then 
    (let socks = List.map (fun x -> x.sock) game_state.players in
     let cur_player = List.nth game_state.players game_state.turn in
     let drawncardlst = drawprint game_state.deck num [] in
     let newState = State.draw game_state num in
     let _ = send ("You have drawn" ^ " " ^ string_of_int num ^ " cards.\n")
         sock in
     let _ = send ("You drew: \n") sock in
     print_hand sock drawncardlst;
     let _ = send ("\n") sock in
     let _ = masssend (cur_player.name ^ " has drawn" ^ " " ^ string_of_int num
                       ^ " cards.\n") socks in newState)
  else (let _ = send ("Sorry, you cannot draw now.\n") sock in game_state)

(** [unohelper game_state s] is the state after the player calls uno. If the 
    player calls uno when they have more than 1 card, they automatically 
    draw 2 cards as a penalty. *)
let unohelper (game_state : State.t) s : State.t = 
  let cur_turn = game_state.turn in
  let cur_player = List.nth game_state.players cur_turn in
  if ((List.length cur_player.hand) > 1 ) 
  then (let _ = send "You don't have Uno.\n" s in
        let newState = drawhelper game_state 2 true s in
        newState)
  else (State.uno game_state)

(** [changeNameHelper x gs s] is the game_state after attempting to change the 
    player's name to [x]. If the player's name cannot be changed to [x] the
    game_state is [gs] and the player is notified that the name change
    failed. *)
let changeNameHelper (x : string) (gs : State.t) s =
  match State.rename_player gs x with 
  | exception State.InvalidName -> 
    let _ = send ("You cannot change your name to " ^ x ^
                  ". " ^ x ^ " is already a player.\n") s in gs
  | newstate -> let socks = List.map (fun x -> x.sock) gs.players in
    let cur_player = List.nth gs.players gs.turn in
    let _ = masssend (cur_player.name ^ "has changed name to " ^ x ^ "\n")
        socks in
    let _ = send ("Name changed to " ^ x ^ "\n") s in newstate

(** [checkWin game_state] checks if the game is over and tells the player if 
    they won or lost. *)
let checkWin (game_state : State.t) =
  (*Check if game is over*)
  let game_status = try State.game_status_one_winner game_state with 
      State.GameOver -> failwith "Does not happen in online mode" in
  let socks = List.map (fun x -> x.sock) game_state.players in
  let winner = List.nth game_state.players 
      ((game_state.turn + 1) mod (List.length game_state.players)) in
  if game_status then
    let _ = masssend ("* " ^ winner.name ^ " won! Game over!\n") socks in
    exit 0

(** [drawHelper2 cur_player game_state can_draw s_descr] is the game when the
    human player attempts to draw a card. *)
let rec drawHelper2 cur_player game_state can_draw s_descr : unit =
  let sock_ur_wearing = (List.nth (game_state.players) game_state.turn).sock in
  (*If the player has 1 card left and doesn't call uno*)
  if (List.length cur_player.hand = 1 && cur_player.unod = false) 
  then let gs = (drawhelper game_state 2 can_draw sock_ur_wearing) in 
    (if gs = game_state
     then repl () gs true false s_descr
     else let _ = 
            send ("\nYou did not call uno, so you had to draw 2 cards.\n") 
              sock_ur_wearing in 
       repl () gs true false s_descr)
  else if (game_state.mode = 2 || game_state.mode = 3) 
  then (repl () (drawhelper game_state 1 can_draw sock_ur_wearing) 
          false true s_descr)
  else (repl () (drawhelper game_state 1 can_draw sock_ur_wearing) 
          true false s_descr)
and 

  (** [passhelper can_pass game_state s_descr cur_sock] is the game state after 
      the player has attempted to pass. *)
  passhelper can_pass game_state s_descr cur_sock = 
  if can_pass then repl () (State.pass game_state) false true s_descr
  else if (game_state.mode = 2) then 
    let _ = send ("You must draw until you can play a card. \n") cur_sock in 
    repl () game_state false true s_descr
  else let _ = send ("You can't pass right now, sorry. \n") cur_sock in 
    repl () game_state false true s_descr
and 

  (** [morePlayHelp x cur_player cur_sock game_state s_descr ] is the game state 
      after the player attempts to play [x]. *)
  morePlayHelp x cur_player cur_sock game_state s_descr =
  (*If the player has 1 card left and doesn't call uno*)
  if (List.length cur_player.hand = 1 && cur_player.unod = false) 
  then (let _ = send ("You did not call uno. Draw 2 cards") cur_sock in
        let _ = send ("\n") cur_sock in 
        repl () (drawhelper game_state 2 true cur_sock) false true s_descr)      
  else if game_state.mode=1 
  then repl () (playhelper game_state x cur_sock) true true s_descr
  else if (game_state.mode = 3) 
  then repl () (playhelper game_state x cur_sock) true false s_descr
  else repl () (playhelper game_state x cur_sock) false true s_descr
and 

  (** [matchCommands game_state input cur_player can_pass can_draw s_descr] *)
  matchCommands game_state input (cur_player : State.player) can_pass can_draw 
    s_descr : unit =
  let sock_ur_wearing = cur_player.sock in
  try
    begin match Parse.read input with 
      | Draw -> drawHelper2 cur_player game_state can_draw s_descr
      | Play x -> morePlayHelp x cur_player sock_ur_wearing game_state s_descr 
      | Uno -> let _ = send "\n" sock_ur_wearing in
        repl () (unohelper game_state sock_ur_wearing) false true s_descr
      | Pass -> passhelper can_pass game_state s_descr sock_ur_wearing
      | Rules x -> let _ = send (x^"\n") sock_ur_wearing in 
        repl () game_state can_pass can_draw s_descr
      | Help x -> let _ = send ("\n"^x^"\n") sock_ur_wearing in 
        repl () game_state can_pass can_draw s_descr
      | ChangeName x -> let _ = send (x^"\n") sock_ur_wearing in 
        repl () (changeNameHelper x game_state sock_ur_wearing) can_pass
          can_draw
          s_descr
      | Quit -> let socks = List.map (fun x -> x.sock) game_state.players in
        masssend "%" socks;
        exit 0
    end
  with (State.InvalidMove) -> 
    (send ("Sorry, invalid move.\n") sock_ur_wearing);
    repl () game_state can_pass can_draw s_descr
and 

  (** [repl () game_state can_pass can_draw s_descr] continues until the game
      ends. It is the main part of the game.
      repl stands for Read Evaluate Print Loop. *)
  repl () (game_state : State.t) (can_pass : bool) (can_draw : bool) 
    (s_descr : file_descr) =
  let cur_turn = game_state.turn in 
  let cur_player = List.nth game_state.players cur_turn in
  let sock_ur_wearing = cur_player.sock in
  checkWin game_state; 
  let _ = send 
      ("\nYou (" ^ (cur_player.name) ^ ") are holding: \n") sock_ur_wearing in
  print_hand sock_ur_wearing cur_player.hand;
  let _ = send "\n\n" sock_ur_wearing in
  let _ = send ("Top Card: ") sock_ur_wearing in 
  let _ = send ((lastplayed_card game_state.discard_pile)) sock_ur_wearing in 
  let _ = send ("\n\n") sock_ur_wearing in 
  let _ = send ("$ What would you like to do next?\n") sock_ur_wearing in 
  try (*Player command*)
    (let input = receive_str (fst sock_ur_wearing) 1000 in
     print_endline input;
     print_newline ();
     matchCommands game_state input cur_player can_pass can_draw s_descr)
  with (Parse.InvalidCommand) -> (
      let _ = send ("Invalid Command: the valid commands begin with " ^ 
                    (Parse.getCommands ", ") ^ "\n") sock_ur_wearing in
      repl () game_state can_pass can_draw s_descr)

(* [init_helper acc lst] transforms all the players in [lst] to non-bots *)
let rec init_helper acc = function
  | [] -> acc
  | h :: t -> init_helper (acc @ [{h with is_bot = false}]) t

(** [play_game gs s_descr] starts a game with 2 players in standard mode *)
let play_game (gs : State.t) (s_descr : file_descr) : unit =
  let game_state = {gs with players = init_helper [] gs.players} in
  let socks = List.map (fun x -> x.sock) game_state.players in
  masssend "Initial Top Card: " socks; 
  masssend ((cardname (Deck.peek game_state.discard_pile))^"\n") socks;
  let game_state2 = try State.init_top_effect game_state
    with State.PendingColor -> 
      begin
        (match read_line () with 
         | exception End_of_file -> game_state
         | (input : string) -> 
           State.change_color game_state (Parse.cardColor [input])) end in
  masssend "Let the games begin!\n" socks;
  masssend "We encourage you to change the name of your player,
   for more information type 'help changename'.\n" socks;
  repl () game_state2 false true s_descr
