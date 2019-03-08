open Deck
open ANSITerminal
open State
(** Main module for running the game, offline. *)

(** [color2string c] is the color of the card represented as a string. If the 
    card is a wild card, the color is Wild. *)
let color2string (c : Deck.color) = 
  match c with 
  | Green -> "Green "
  | Red -> "Red "
  | Blue -> "Blue "
  | Yellow -> "Yellow "
  | Wild -> "Wild"

(**[color2ansi c] takes in a deck.color and returns the ANSITerminal color for 
   the color of each card.*)
let color2ansi (c : Deck.color) : ANSITerminal.style list = 
  match c with 
  | Green -> [ANSITerminal.green]
  | Red -> [ANSITerminal.red]
  | Blue -> [ANSITerminal.blue]
  | Yellow -> [ANSITerminal.yellow]
  | Wild -> [ANSITerminal.magenta]

(** [cardnum2string i] is the card number or action represented as a string. 
    Example: [cardnum2string 9] is ["9"], [cardnum2string 12] is ["Reverse"]. *)
let cardnum2string  = function
  | x when x < 10 -> string_of_int x
  | 10 -> "Skip"
  | 11 -> "PlusTwo"
  | 12 -> "Reverse"
  | _ -> failwith "Not a valid card number so impossible"

(** [print_hand lst] prints the cards in the current player's hand. *)
let rec print_hand = function
  | [] -> ()
  | {color = x; value = y; card_type = z}::t -> 
    begin match x, y with 
      | Wild, -1 -> print_string [magenta] "Wild, "; print_hand t
      | Wild, -2 -> print_string [magenta] "Wild PlusFour, "; print_hand t
      | _, _ -> print_string (color2ansi x) (color2string x); 
        print_string (color2ansi x) (cardnum2string y);
        (if t = [] then print_string (color2ansi x) "" 
         else print_string (color2ansi x) ", "; print_hand t)
    end 

(** [print_boldhand lst] prints the cards in the current players hand but 
    bold.*)
let rec print_boldhand = function
  | [] -> ()
  | {color = x; value = y; card_type = z}::t -> 
    begin match x, y with 
      | Wild, -1 -> print_string [Bold; magenta] "Wild, "; print_hand t
      | Wild, -2 -> print_string [Bold; magenta] "Wild PlusFour, "; print_hand t
      | _, _ -> print_string (Bold::(color2ansi x)) (color2string x); 
        print_string (Bold::(color2ansi x)) (cardnum2string y);
        (if t = [] then print_string (Bold::(color2ansi x)) "" 
         else print_string (Bold::(color2ansi x)) ", "; print_hand t)
    end 

(** [lastplayed_card discPile] is the most recent card played in the game. 
    In other words, it is the top card of discard pile. *)
let lastplayed_card = function
  | [] -> ""
  | {color = x; value = y; card_type = z}::t -> 
    (color2string x) ^ (
      begin match y with
        | -1 -> "Wild"
        | -2 -> "PlusFour"
        | _ -> (cardnum2string y)
      end)

(** [lastplayed_Deckcard discPile] is the color of the card last played 
    that is on top of the discard pile. *)
let lastplayed_Deckcard  = function
  | [] -> []
  | {color = x; value = y; card_type = z}::t -> color2ansi x

(** [cardname card] is the display name of [card] as a string. *)
let cardname = function 
  | {color = x; value = y; card_type = z} -> 
    (color2string x) ^ (begin match y with
        | -1 -> "Wild" 
        | -2 -> "PlusFour"
        |  _ -> (cardnum2string y)
      end) 

(** [playhelperPlusFour game_state x] is the state of the game after the player
    plays a plusfour. *)
let playhelperPlusFour game_state x curplayer =
  begin match 
      (State.play (State.change_wildcard_color game_state 
                     {color = Wild; value = -2; card_type = "PlusFour"} 
                     (x.color)) x) with
  | exception State.InvalidMove -> 
    (print_string [red] "Sorry that move is Invalid. Try again."; 
     print_newline (); game_state)
  | a -> print_string (color2ansi x.color) 
           ((Parse.displayCard "PlusFour.txt") ^ "\n");
    print_string [cyan; Bold] (curplayer.name ^ " (You) played a PlusFour\n\n");
    let next_player = List.nth game_state.players 
        ((a.turn + 1)  mod (List.length game_state.players)) in 
    (print_string [white] "Color changed to ";
     print_string (color2ansi x.color) ((color2string x.color) ^ ". ");
     print_newline ();
     print_string [Bold; cyan] (next_player.name);
     print_string [cyan] " has drawn four cards.";
     print_newline (); a)
  end

(** [playhelperWild game_state x] is the state of the game after the player
    plays a Wild. *)
let playhelperWild game_state x curplayer =
  begin match
      State.play (State.change_wildcard_color game_state 
                    {color = Wild; value = -1; card_type = "Wild"} 
                    (x.color)) x with 
  | exception State.InvalidMove -> 
    print_string ([red]) "Sorry that move is Invalid. Try again."; 
    print_newline (); game_state 
  | a -> 
    (print_string [cyan; Bold] (curplayer.name ^ " (You) played a \n");
     print_string (color2ansi x.color) ((Parse.displayCard "Wild.txt") ^ "\n");
     print_newline ();
     print_string ([white])("Color changed to ");
     print_string (color2ansi x.color) (color2string x.color); a)
  end
(** [playSkip state nextplayer] is the state after the player successfully
    plays a Skip. *)
let playSkip state nextplayer x =
  print_string (color2ansi x.color) ((color2string x.color) ^ " Skip");
  print_newline (); 
  print_string (color2ansi x.color) ((Parse.displayCard "Skip.txt") ^ "\n");
  print_newline ();
  print_string ([Bold; cyan]) (nextplayer.name ^ "'s turn has been skipped."); 
  print_newline (); state

(** [playPlusTwo game_state nextstate x nextplayer] is the state after the 
    player successfully plays a PlusTwo. *)
let playPlusTwo game_state nextstate x nextplayer =
  print_string (color2ansi x.color) ((color2string x.color) ^ " PlusTwo");
  print_newline (); 
  print_string (color2ansi x.color) ((Parse.displayCard "PlusTwo.txt") ^ "\n");
  print_newline ();
  if (game_state.mode <> 1 && game_state.mode <> 3) 
  then (print_string ([Bold; cyan]) (nextplayer.name ^ 
                                     " has drawn 2 cards and lost their turn."); 
        print_newline (); nextstate) else nextstate

(** [playReverse state x prev_player] is the state [t'] after the player has 
    successfully played a Reverse. *)
let playReverse state x prev_player =
  print_string (color2ansi x.color) ((color2string x.color) ^ " Reverse");
  print_newline (); 
  print_string (color2ansi x.color)
    ((Parse.displayCard "Reverse.txt") ^ "\n");
  print_newline ();
  print_string ([Bold; cyan]) 
    ("The order has been reversed. It is now " ^ 
     prev_player.name ^ "'s turn."); 
  print_newline (); state

(** [playhelperNormal game_state x] is the state of the game after any card
    that is neither a Wild nor a PlusFour is played. *)
let playhelperNormal game_state x curplayer =
  begin match State.play game_state x with
    | exception State.InvalidMove -> 
      print_string ([red]) "Sorry that move is Invalid. Try again."; 
      game_state 
    | a -> 
      let nextTurn = (game_state.turn + 1) mod 
                     (List.length game_state.players) in
      let prevTurn = (game_state.turn + 
                      (List.length game_state.players) - 1) mod 
                     (List.length game_state.players) in
      let nextplayer = List.nth (game_state.players) nextTurn in
      let prev_player = List.nth (game_state.players) prevTurn in
      print_newline (); 
      print_string [cyan; Bold] (curplayer.name ^ " (You) played a ");

      begin match x.value with 
        | 10 -> playSkip a nextplayer x
        | 11 -> playPlusTwo game_state a x nextplayer
        | 12 -> playReverse a x prev_player
        | _ -> print_string (color2ansi x.color) 
                 ((color2string x.color) ^ (cardnum2string x.value));
          print_newline (); a
      end 
  end

(** [playhelper game_state x] is the state of the game after a card is played.
    If the move is invalid, it prompts for the user to input another 
    command. *)
let playhelper (game_state : State.t) (x : Deck.card) = 
  let curplayer = List.nth game_state.players game_state.turn in
  print_newline (); 
  print_string ([white]) "You chose to play ";
  print_string (color2ansi x.color) (cardname x);
  print_newline ();
  begin match x.card_type with 
    | "Wild" -> playhelperWild game_state x curplayer
    | "PlusFour" -> playhelperPlusFour game_state x curplayer
    | _ -> playhelperNormal game_state x curplayer
  end 

(**[drawprint dec num acc] returns list of the cards that will be drawn from 
   the top of the deck. *)
let rec drawprint (deck: Deck.card list) (num: int) acc = 
  if num = 0 then acc else 
    match deck with 
    | [] -> acc
    | h::t -> h::(drawprint t (num-1) acc)

(** [drawhelper game_state num] is the state after the user draws [num] 
    cards. *)
let drawhelper (game_state : State.t) (num : int) (can_draw : bool) = 
  if can_draw then (
    let drawncardlst = drawprint game_state.deck num [] in
    let newState = State.draw game_state num in
    print_string ([white]) 
      ("You have drawn" ^ " " ^ string_of_int num ^ " cards.");
    print_newline();
    print_string ([white]) "You drew ";
    print_boldhand drawncardlst;
    print_newline (); newState)
  else (print_string ([red]) "Sorry, you cannot draw now."; print_newline ();
        game_state)

(** [unohelper game_state] is the state after the player calls uno. If the 
    player calls uno when they have more than 1 card, they automatically 
    draw 2 cards as a penalty. *)
let unohelper (game_state : State.t) = 
  let cur_player = game_state.turn |> List.nth game_state.players in
  if ((List.length cur_player.hand) > 1 ) 
  then (print_string ([white]) "You don't have Uno.";
        print_newline();
        let newState = drawhelper game_state 2 true in
        newState)
  else (State.uno game_state)

(** [changeNameHelper x gs] is the game_state after attempting to change the 
    player's name to [x]. If the player's name cannot be changed to [x] the
    game_state is [gs] and the player is notified that the name change
    failed. *)
let changeNameHelper (x : string) (gs : State.t) =
  match State.rename_player gs x with 
  | exception State.InvalidName -> 
    print_string ([white]) ("You cannot change your name to " ^ x ^
                            ". " ^ x ^ " is already a player."); 
    print_newline (); gs
  | newstate -> print_string ([white]) ("Name changed to " ^ x); 
    print_newline (); newstate

(** [custom_mod x y] is the "mod" operation that handles negative [x]'s. *)
let rec custom_mod (x : int) (y : int) = 
  if (x > 0) then (x mod y) else (custom_mod (x + y) y)

(** [phelper ] is the top-ten list of player high scores with the current
    player's score inserted if it is in the top ten. 
    Note, this list is in order from lowest to highest score. *)
let rec phelper acc beat name sPoints points = function 
  | [] -> let len= List.length acc in 
    if len = 0 then 
      (print_string ([red]) (name ^ " : " ^ (sPoints));  print_newline ();
       [(name ^ "=" ^ (sPoints))]) 
    else if (len < 10 && beat = false) 
    then (print_string ([red]) (name ^ " : " ^ (sPoints));  
          print_newline (); (name ^ "=" ^ (sPoints))::acc) else acc
  | n::s::t -> if (List.length acc = 10) then acc else 
      begin try
          if (int_of_string s) >= points 
          then (print_string ([white]) (n ^ " : " ^ s); 
                print_newline ();
                phelper ((n ^ "=" ^ s)::acc) false name sPoints points t)
          else if beat 
          then (print_string ([white]) (n ^ " : " ^ s);
                print_newline (); 
                phelper ((n ^ "=" ^ s)::acc) true name sPoints points t)
          else (print_string ([red]) (name ^ " : " ^ (sPoints)); 
                print_newline (); print_string ([white]) 
                  (n ^ " : " ^ s); 
                print_newline (); 
                phelper ((n ^ "=" ^ s)::(name ^ "=" ^ sPoints)::acc) 
                  true name sPoints points t)
        with (Failure _) -> phelper acc false name sPoints points  t end
  | _ -> acc 

(** [points_helper points name] displays the high score leaderboard and adds
    the current player's score to the leaderboard if it is in the top 10. *)
let points_helper points name =
  print_newline (); 
  let scores = Parse.highScore 
               |> List.map (fun a -> String.split_on_char '=' a)
               |> List.flatten in 
  print_string ([white; Bold]) "High Scores:";
  print_newline ();
  let sPoints = string_of_int points in 
  phelper [] false name sPoints points scores 
  |> List.rev 
  |> Parse.updateHighScores 

(** [checkWin game_state] checks if the game is over and tells the player if 
    they won or lost. *)
let checkWin (game_state : State.t) =
  (*Check if game is over*)
  let game_status = try State.game_status_one_winner game_state with 
      State.GameOver -> 
      (print_string ([red]) "Game Over, you lose. Please play again!"; 
       let points = State.get_winner_points game_state in
       print_newline (); print_newline ();
       print_string ([red])  ((State.get_winner_name game_state) ^ " received: "
                              ^ (string_of_int points) ^ " points");
       points_helper points (State.get_winner_name game_state); 
       print_newline(); exit 0) in 
  if game_status then let points = State.get_winner_points game_state in
    (print_string ([green]) "Congrats! You win! Great job!"; 
     print_newline ();
     print_string ([green]) 
       ("You recieved: " ^ (string_of_int points) ^ " points");
     print_newline (); 
     points_helper points (State.get_winner_name game_state); 
     print_newline (); exit 0)

(** [botstate] is the autoplay the bot uses depending on the difficulty of the
    bot. *)
let botstate game_state = 
  match game_state.difficulty with 
  | 0 -> State.worseautoplay game_state 
  | 1 -> State.auto_play game_state
  | 2 -> State.betterautoplay game_state
  | _ -> State.cheatingautoplay game_state

(** [skipPlayerTurn newstate game_state cur_player] prints the message telling 
    the player that their turn has been lost. *)
let skipPlayerTurn newstate game_state cur_player = 
  let new_player = List.nth newstate.players game_state.turn in
  let op_hand = List.length new_player.hand in
  print_string ([Bold; cyan]) (cur_player.name); print_string ([white]) 
    (" has " ^ (string_of_int op_hand) ^ " cards left.");
  print_newline ()

(** [botPlayWild lastCol newstate cur_player] displays the message explaining
    to the player that the bot has played a Wild. *)
let botPlayWild lastCol newstate cur_player =
  print_string (color2ansi lastCol) "Wild";
  print_string (lastplayed_Deckcard newstate.discard_pile) 
    (Parse.displayCard "Wild.txt"); print_newline (); 
  let new_player =  List.nth newstate.players 
      (custom_mod (newstate.turn - 1) (List.length newstate.players)) in
  let op_hand = List.length new_player.hand in
  print_string ([Bold; cyan]) (cur_player.name); print_string 
    ([white]) (" has " ^ (string_of_int op_hand) ^ " cards left.");
  print_newline ()

(** [botPlayPlus4 lastCol newstate next_player game_state cur_player] displays 
    the message explaining to the player that the bot has played a PlusFour. *)
let botPlayPlus4 lastCol newstate next_player game_state cur_player=
  print_string (color2ansi lastCol) ("PlusFour");
  print_string (lastplayed_Deckcard newstate.discard_pile) 
    (Parse.displayCard "PlusFour.txt"); print_newline ();
  if (next_player.is_bot = false) then 
    (let newhand = drawprint game_state.deck 4 [] in
     print_string ([Bold; cyan]) ("You have drawn: ");
     print_boldhand newhand; print_newline (); print_newline ())
  else 
    (print_string ([Bold; cyan]) (next_player.name ^ " has drawn 4 cards.");
     print_newline (); print_newline (); 
     skipPlayerTurn newstate game_state cur_player)

(** [botPlaySkip lastCol newstate next_player game_state cur_player] displays 
    the message explaining to the player that the bot has played a Skip. *)
let botPlaySkip lastCol newstate next_player game_state cur_player =
  print_string (color2ansi lastCol) ("Skip\n");
  print_string (lastplayed_Deckcard newstate.discard_pile) 
    (Parse.displayCard "Skip.txt"); print_newline ();
  print_string ([Bold; cyan])
    (next_player.name ^ "'s turn has been skipped.");
  print_newline (); print_newline (); 
  skipPlayerTurn newstate game_state cur_player

(** [botPlayPlusTwo lastCol newstate next_player game_state cur_player] displays 
    the message explaining to the player that the bot has played a PlusTwo. *)
let botPlayPlusTwo lastCol newstate next_player game_state cur_player =
  print_string (color2ansi lastCol) "PlusTwo\n";
  print_string (lastplayed_Deckcard newstate.discard_pile) 
    (Parse.displayCard "PlusTwo.txt"); print_newline (); 
  if ((next_player.is_bot = false) && game_state.mode <> 1 
      && game_state.mode <> 3) then 
    let newhand = drawprint game_state.deck 2 [] in
    (print_string ([Bold; cyan]) ("You have drawn: "); print_boldhand newhand;
     print_newline (); print_newline ())
  else if ((next_player.is_bot) && game_state.mode <> 1 
           && game_state.mode <> 3) then 
    (print_string ([Bold; cyan]) (next_player.name ^ " has drawn two cards.");
     print_newline (); print_newline (); 
     skipPlayerTurn newstate game_state cur_player)

(** [botPlayReverse lastCol newstate prev_player game_state cur_player] displays 
    the message explaining to the player that the bot has played a Reverse. *)
let botPlayReverse lastCol newstate prev_player game_state cur_player =
  print_string (color2ansi lastCol) "Reverse\n";
  print_string (lastplayed_Deckcard newstate.discard_pile) 
    (Parse.displayCard "Reverse.txt"); print_newline ();
  print_string ([Bold; cyan])("The order has been reversed. It is now " ^
                              prev_player.name ^ "'s turn");
  print_newline (); print_newline ();
  let new_turn = ((List.length newstate.players) - game_state.turn - 1)
                 mod (List.length newstate.players) in
  let new_player = List.nth newstate.players new_turn in
  let op_hand = List.length new_player.hand in
  print_string ([Bold; cyan]) (cur_player.name); 
  print_string ([white]) (" has " ^ (string_of_int op_hand) ^ " cards left.");
  print_newline ()

(** [botPlayNumCard lastCol newstate lastplayed cur_player] displays the message
    explaining to the player that the bot has played a number card. *) 
let botPlayNumCard lastCol newstate lastplayed cur_player =
  print_string (lastplayed_Deckcard newstate.discard_pile)  
    (lastplayed_card newstate.discard_pile);
  let new_player = List.nth newstate.players 
      (custom_mod (newstate.turn - 1) (List.length newstate.players)) in
  let op_hand = List.length new_player.hand in
  print_newline (); print_newline (); 
  print_string ([Bold; cyan]) (cur_player.name); print_string 
    ([white]) (" has " ^ (string_of_int op_hand) ^ " cards left.");
  print_newline ()

(** [tryPlayBot game_state cur_player] is the state of the game after the 
    AI attempts to play a card. 
    Raises: [State.AutoDraw] if the bot is forced to draw a card. *)
let rec tryPlayBot game_state cur_player =
  let newstate = botstate game_state in
  print_string ([Bold; cyan]) (cur_player.name);
  print_string ([white]) " played: ";
  let lastplayed = (List.hd newstate.discard_pile).value in 
  let lastCol = (List.hd newstate.discard_pile).color in
  let nextTurn = (game_state.turn + 1) mod (List.length game_state.players) in
  let prevTurn = (game_state.turn + (List.length game_state.players) - 1)
                 mod (List.length game_state.players) in
  let next_player = List.nth (game_state.players) nextTurn in
  let prev_player = List.nth (game_state.players) prevTurn in
  begin match lastplayed with 
    | -1 -> botPlayWild lastCol newstate cur_player 
    | -2 -> botPlayPlus4 lastCol newstate next_player game_state cur_player;
    | 10 -> botPlaySkip lastCol newstate next_player game_state cur_player;
    | 11 -> botPlayPlusTwo lastCol newstate next_player game_state cur_player;
    | 12 -> botPlayReverse lastCol newstate prev_player game_state cur_player; 
    | _ -> botPlayNumCard lastCol newstate lastplayed cur_player;
  end;
  print_newline ();
  repl newstate false true
and 

  (* If the ai plays a plusfour or plustwo, what the player draws must be
        displayed. If the ai plays skip or reverse, the new player must be 
        stated. *)
  (** [playBot game_state] is the game state after a bot plays. *)
  playBot game_state (cur_player : State.player) =
  try tryPlayBot game_state cur_player
  with (State.AutoDraw) -> 
    let newstate = if (game_state.difficulty <> 3) 
      then State.pass (State.draw game_state 1) 
      else State.pass (State.cheatdraw game_state 1) in
    print_string ([Bold; cyan]) (cur_player.name); print_string ([white])
      (" drew 1 card."); print_newline (); print_newline ();
    let new_player = List.nth newstate.players 
        (custom_mod (newstate.turn - 1) (List.length newstate.players)) in
    let op_hand = List.length new_player.hand in
    print_string ([Bold; cyan]) (cur_player.name); print_string ([white])
      (" has " ^ (string_of_int op_hand) ^ " cards left."); print_newline ();
    let try_st = {newstate with turn = (custom_mod (newstate.turn - 1) 
                                          (List.length newstate.players))} in
    try (let _ = botstate try_st in repl try_st true false)
    with (AutoDraw) -> (
        if (game_state.mode = 0 || game_state.mode = 1) then
          (repl newstate false true) 
        else
          repl try_st false true )
and 

  (** [drawHelper2 cur_player game_state can_draw] is the game when the human
      player attempts to draw a card. *)
  drawHelper2 cur_player game_state can_draw =
  (*If the player has 1 card left and doesn't call uno*)
  if (List.length cur_player.hand = 1 && cur_player.unod = false) 
  then let gs = (drawhelper game_state 2 can_draw) in 
    if gs = game_state
    then repl gs true false
    else (print_newline ();
          (print_string (([white]))
             "You did not call uno, so you had to draw 2 cards.");
          print_newline ();
          repl gs true false)
  else if (game_state.mode = 2 || game_state.mode = 3) 
  then (repl (drawhelper game_state 1 can_draw) false true)
  else (repl (drawhelper game_state 1 can_draw) true false)
and 

  (** [passhelper can_pass game_state] is the game_state after the current 
      player passes. *)
  passhelper can_pass game_state =
  if can_pass then repl (State.pass game_state) false true
  else if (game_state.mode = 2) then 
    (print_string [Bold; red] 
       ("You must draw until you can play a card.");
     print_newline ();
     repl game_state false true)
  else 
    ((print_string  [white])
       "You can't pass right now, sorry.");
  print_newline ();
  repl game_state false true
and

  (** [matchCommands game_state] *)
  matchCommands game_state input (cur_player : State.player) can_pass can_draw =
  begin match Parse.read input with 
    | Draw -> drawHelper2 cur_player game_state can_draw 
    | Play x -> (*If the player has 1 card left and doesn't call uno*)
      if (List.length cur_player.hand = 1 && cur_player.unod = false) 
      then ((print_string ([white]) 
               "You did not call uno. Draw 2 cards"); print_newline ();
            repl (drawhelper game_state 2 true) false true)
      else if game_state.mode = 1 
      then repl (playhelper game_state x) true true 
      else if (game_state.mode = 3) 
      then (repl (playhelper game_state x) true false) 
      else repl (playhelper game_state x) false true
    | Uno -> print_newline (); repl (unohelper game_state) false true
    | Pass -> passhelper can_pass game_state
    | Rules x -> print_string ([white]) x; print_newline ();
      repl game_state can_pass can_draw
    | Help x -> print_newline (); print_string ([white]) x; 
      print_newline (); repl game_state can_pass can_draw
    | ChangeName x -> print_newline (); 
      repl (changeNameHelper x game_state) can_pass can_draw
    | Quit -> exit 0
  end
and 

  (** [bot_playing game_state can_pass can_draw cur_player] is the game when 
      it is the AI's turn to play. *)
  bot_playing game_state can_pass can_draw cur_player =
  try playBot game_state cur_player
  with (State.TakeStack) -> 
    let temp_st = State.draw game_state (2 * (List.length game_state.stack)) in
    let after_draw = 
      {temp_st with 
       stack = []; 
       turn = (game_state.turn + 1) mod (List.length game_state.players);
       discard_pile = temp_st.stack @ temp_st.discard_pile} in
    let cur_num = (List.length 
                     (List.nth after_draw.players game_state.turn).hand) in
    let prev_num = (List.length 
                      (List.nth game_state.players game_state.turn).hand) in
    let diff = cur_num - prev_num in
    if (game_state.mode = 1 && diff >= 2) || (game_state.mode = 3 && diff >= 2) 
    then let new_turn = custom_mod (after_draw.turn - 1)
             (List.length after_draw.players) in
      let new_player = List.nth after_draw.players new_turn in
      let stack_size = string_of_int (List.length game_state.stack) in
      let op_hand = (List.length new_player.hand) in
      print_string ([Bold; cyan]) (cur_player.name);
      print_string ([white]) (" has taken the stack of " ^ stack_size ^
                              " PlusTwo's and has " ^ 
                              (string_of_int op_hand) ^ " cards left.");
      print_newline();
      repl after_draw false true
and 

  (** [repl game_state can_pass] continues until the game ends. 
      It is the main part of the game.
      repl stands for Read Evaluate Print Loop. *)
  repl (game_state : State.t) (can_pass : bool) (can_draw : bool) =
  print_newline ();
  let cur_turn = game_state.turn in 
  let cur_player = List.nth game_state.players cur_turn in
  checkWin game_state; 
  (*Check whether current player is bot or human and then acts accordingly*)
  if cur_player.is_bot then bot_playing game_state can_pass can_draw cur_player
  else (*When the current player is human*)
    (print_string ([yellow]) ("You (" ^ (cur_player.name) ^ ") are holding:");
     print_newline (); print_hand cur_player.hand;
     print_newline (); print_newline ();
     print_string ([Bold; white])"Top Card: ";  
     print_string (Bold :: lastplayed_Deckcard game_state.discard_pile)
       (lastplayed_card game_state.discard_pile);
     print_newline (); print_newline ();
     print_string ([white]) "What would you like to do next?";
     print_newline ();
     try (match read_line () with 
         | exception End_of_file -> ()
         | (input : string) -> 
           matchCommands game_state input cur_player can_pass can_draw)
     with (Parse.InvalidCommand) ->
       (print_string ([red]) 
          ("Invalid Command: the valid commands begin with " ^ 
           (Parse.getCommands ", ")); 
        print_newline (); repl game_state can_pass can_draw))

(** [play_game num] starts the game with [num] players, difficulty [dif] and
    gamemode [gm] *)
let play_game (num : int) (dif : int) (gm : int) = 
  let game_state = State.init_game num gm dif in
  print_string ([Bold; white]) "Initial Top Card: "; 
  print_string  (Bold::lastplayed_Deckcard (game_state.discard_pile))
    (cardname (Deck.peek game_state.discard_pile)); 
  print_newline ();
  let game_state2 = try State.init_top_effect game_state
    with State.PendingColor -> 
      let cur_player = List.nth game_state.players game_state.turn in
      if (cur_player.is_bot) then (
        let temp_st = State.auto_play_wild game_state in
        let color = (Deck.peek temp_st.discard_pile).color in
        print_string ([Bold; cyan]) cur_player.name;
        print_string ([white]) " changed the color to ";
        print_string (color2ansi color) (color2string color);
        print_newline ();
        temp_st) 
      else begin
        (match read_line () with 
         | exception End_of_file -> game_state
         | (input : string) -> 
           State.change_color game_state (Parse.cardColor [input])) end in
  print_string ([white]) "Let the games begin!\n";
  print_string ([white]) "We encourage you to change the name of your player,
for more information type 'help changename'.";
  print_newline (); repl game_state2 false true 

(** [let chooseMode n x] prompts for a game mode. *)
let rec chooseMode n x =
  print_string [white] "What game mode would you like to play on?\nThe choices 
are 'Standard' (basic uno rules), 'Stacking' (can stack plustwos, and 
cards with the same number), 'Draw Forever' (you must draw cards until you 
have a playable card), and 'Dos' (combination of 'Stacking' and
'Draw Forever')."; print_newline ();
  try
    Pervasives.print_string "> ";
    let i = read_line () in 
    match i with
    | exception End_of_file -> () 
    | a when (String.lowercase_ascii a) = "quit" -> exit 0
    | a ->  play_game n x (Parse.readMode a)
  with (Parse.InvalidCommand) ->
    print_string [Bold; red] "I'm sorry,
    that is not a valid game mode. Please choose from 'Standard', 'Stacking', 
    'Draw Forever', and 'Dos'."; print_newline ();
    chooseMode n x

(** [chooseDif n] prompts for a difficulty level for the game. *)
let rec chooseDif n =
  print_string [white] "What difficulty would you like to play on?\nThe choices
are 'Easy', 'Medium', 'Hard', or 'Supreme'.";
  try
    print_newline ();
    Pervasives.print_string "> ";
    let input = read_line () in 
    match input with 
    | exception End_of_file -> ()
    | a when (String.lowercase_ascii a) = "quit" -> exit 0
    | a -> chooseMode n (Parse.readDif a)
  with (Parse.InvalidCommand) -> 
    print_string [Bold; red] "I'm sorry, that is not a valid game mode. 
    Please choose from 'Easy', 'Medium', 'Hard', or 'Supreme'.";
    print_newline ();
    chooseDif n

(**[ main ()] starts the game and prompts for the number of players. *)
let rec main () = 
  try
    print_endline "Please enter the number of players you have. (2-5) \n";
    Pervasives.print_string  "> ";
    let temp = read_line () in
    match temp with
    | exception End_of_file -> ()
    | a when (String.lowercase_ascii a) = "quit" -> exit 0
    | a when (a = "1") -> raise (Failure "Can't be 1")
    | n -> chooseDif (int_of_string n)
  with (Failure _) -> 
    print_string ([red])
      "Invalid number of players. Please enter an integer, or 'quit' 
   to terminate the game. Number of players has to be at least 2.";
    print_newline(); main ()

