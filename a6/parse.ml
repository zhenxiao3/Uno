(** Parse.ml reads player's input and reads and writes to files. *)
(** Type to represent the possible uno commands. *)
type command = 
  | Quit
  | Draw
  | Uno 
  | Play of Deck.card 
  | Pass
  | Rules of string 
  | Help of string 
  | ChangeName of string 

(** Exception for commands that don't match the above pattern and therefore
    aren't valid. *)
exception InvalidCommand

(** [del_space acc s] is the string list [s] with all instances of [""] 
    removed. All strings in [del_space acc s] consist solely of lowercase 
    letters, regardless of any capital letters in the original list [s]. *)
let rec del_space acc s =
  match s with 
  | [] -> acc
  | h::t -> if h = "" then del_space acc t
    else del_space ((String.lowercase_ascii h)::acc) t 

(** [cardColor t] is the [Deck.Color] of the card the player enters. 
    Raises: [InvalidCommand] if the string isn't a valid color. *)
let cardColor = function
  | [] -> raise InvalidCommand
  | "blue"::t -> Deck.Blue 
  | "red"::t -> Deck.Red 
  | "green"::t -> Deck.Green 
  | "yellow"::t -> Deck.Yellow 
  | "wild"::t 
  | "plusfour"::t -> Deck.Wild 
  | _ -> raise InvalidCommand

(** [reader acc file] is the string representing the contents of [file],
    where [file] is an open in_channel. *)
let rec reader acc file =
  match (input_line file) with
  | exception End_of_file -> acc
  | s -> reader (acc ^ "\n" ^ s) file

(** [displayCard f] is the string representation of the contents of the file 
    [f]. Used for ASCII art. *)
let displayCard f =
  let file = open_in f in 
  let return = reader "" file in  
  close_in file;
  return 

(** [ruleshelper] is the string representation of the rules of the game,
    which is the contents of the file "Rules.txt". *)
let ruleshelper =
  let file = open_in "Rules.txt" in 
  let rules = reader "" file in 
  close_in file; 
  rules

(** [wildColor t] is the [Deck.Color] the player wishes to change the current
    color to after playing a Wild or a Wild PlusFour. 
    Raises: [InvalidCommand] if the color isn't a valid color. *)
let wildColor t = 
  match cardColor t with 
  | Deck.Wild -> raise InvalidCommand
  | h -> h

(** [cardNumber t] is the number or action on the card played. *)
let cardNumber = function  
  | "0"::t -> 0
  | "1"::t -> 1
  | "2"::t -> 2
  | "3"::t -> 3
  | "4"::t -> 4
  | "5"::t -> 5
  | "6"::t -> 6
  | "7"::t -> 7
  | "8"::t -> 8
  | "9"::t -> 9
  | "skip"::t -> 10
  | "plustwo"::t -> 11
  | "reverse"::t -> 12
  | _ -> raise InvalidCommand

(** [find w1 w2 acc b l] is the list [l] from [w1] to [w2], not including [w2]. 
    [b] is true if w1 has been found.
    The list returned is in reverse order. *)
let rec find w1 w2 (acc : 'a list) b = function 
  | [] -> acc 
  | h::t -> begin if b then 
        (if h = w2 then (h::acc) else find w1 w2 (h::acc) b t)
      else (if h = w1 then find w1 w2 
                ((String.sub h 2 ((String.length h) - 2))::acc) true t 
            else find w1 w2 acc b t) end

(** [getInstructions s com] is the string of the section in "Rules.txt" that 
    pertains to [s].
    [com] is true if [s] is a command, false if [s] is a card. *)
let getInstructions s com =
  let file= open_in (if com then "Commands.txt" else "Rules.txt") in 
  let final = reader "" file
              |> String.split_on_char ' '  
              |> find ("\n\n" ^ s) "sensitive." [] false 
              |> List.rev 
              |> String.concat " " in 
  close_in file;
  final

(** [commandsHelper acc lst] is the string representing all the words at the
    start of paragraphs and enclosed in [<>] in the string list [lst]. *)
let rec commandsHelper acc = function 
  | [] -> acc
  | h::t -> let len = String.length h in 
    if len < 4 then commandsHelper acc t 
    else if (((String.sub h 0 3) = "\n\n<") && 
             ((String.sub h (len - 1) 1) = ">")) 
    then commandsHelper ((String.sub h 3 (len - 4)) :: acc) t
    else commandsHelper acc t

(** [getCommands sep] is the list of commands in the game. 
    [sep] is how they are parsed together. 
    The list of commands is pulled from [Commands.txt] so that when that
    file is updated, the command list is automatically updated. *)
let getCommands sep=
  let file = open_in "Commands.txt" in 
  let commands = reader "" file in 
  let l = String.split_on_char ' ' commands in 
  close_in file; 
  commandsHelper [] l
  |> List.filter (fun a -> a <> "Help")
  |> List.append [getInstructions "<Help>" true; "For more help, use"]
  |> List.rev
  |> String.concat sep 

(** [highScore] is the list of player high scores. *)
let highScore =
  let file = open_in "Score.txt" in
  let scores = reader "" file in 
  let toReturn = List.filter (fun x -> x <> "") 
      (String.split_on_char '\n' scores) in 
  close_in file;
  toReturn 

(** [updateHelper channel lst] writes each element of [lst] to [channel]. *)
let rec updateHelper channel = function 
  | [] -> ()
  | h::t -> output_string channel (h ^ "\n"); updateHelper channel t

(** [updateHighScores lst] updates the saved high score list. *)
let updateHighScores lst =
  let file = open_out "Score.txt" in 
  updateHelper file lst; 
  close_out file

(** [helphelper t] is the string the player has requested, to help them 
    understand the rules.
    Raises: [InvalidCommand] if [t] is not a valid command. *)
let helphelper = function
  | [] -> failwith "Impossible"
  | h::[] -> begin match h with 
      | "skip" -> getInstructions "Skip:" false
      | "reverse" -> getInstructions "Reverse:" false
      | "plustwo" -> getInstructions "PlusTwo:" false
      | "wild" -> getInstructions "Wild:" false 
      | "plusfour" -> getInstructions "PlusFour:" false
      | "0"
      | "1"
      | "2"
      | "3"
      | "4"
      | "5"
      | "6"
      | "7"
      | "8"
      | "9" -> getInstructions "NumberCards:" false
      | "help" -> getInstructions "<Help>" true
      | "play" -> getInstructions "<Play>" true 
      | "uno" -> getInstructions "<Uno>" true 
      | "draw" -> getInstructions "<Draw>" true
      | "rules" -> getInstructions "<Rules>" true 
      | "quit" -> getInstructions "<Quit>" true
      | "pass" -> getInstructions "<Pass>" true 
      | "changename" -> getInstructions "<ChangeName>" true 
      | _ -> raise InvalidCommand
    end 
  | h::t -> raise InvalidCommand

(** [createCard t] is the card parsed from the string list [t]. 
    Raises: [InvalidCommand] if the phrase is not a valid card. *)
let createCard t : Deck.card =
  if List.length t <> 2 then raise InvalidCommand 
  else let col = cardColor t in
    match col with 
    | Deck.Wild when (List.hd t) = "wild" ->  
      {color = (wildColor (List.tl t)); value = -1; card_type = "Wild"}
    | Deck.Wild when (List.hd t) = "plusfour" -> 
      {color = (wildColor (List.tl t)); value = -2; card_type = "PlusFour"}
    | c -> begin match cardNumber (List.tl t) with
        | x when x < 10 -> {color = c; value = x; card_type = "NumberCard"}
        | x when x = 11 -> {color = c; value = x; card_type = "PlusTwo"}
        | x -> {color = c; value = x; 
                card_type = (List.tl t |> List.hd |> String.capitalize_ascii)}
      end

(** [namehelper t] is the string the player wants their name to be changed to. 
    Note: All words in the name will be capitalized. *)
let namehelper t =
  List.map (fun a -> String.capitalize_ascii a) t 
  |> String.concat " "

(** [read s] is the uno command form of the player's input. 
    Raises: [InvalidCommand] if the string entered isn't a valid command. *) 
let read s =
  let wordlist = s |> String.split_on_char ' ' |> del_space [] |> List.rev in 
  match wordlist with 
  | [] -> raise InvalidCommand
  | h::t when t = [] ->  begin match h with 
      | "draw" -> Draw 
      | "uno" -> Uno 
      | "pass" -> Pass 
      | "quit" -> Quit
      | "rules" -> Rules ruleshelper
      | "help" -> Help (getCommands "\n")
      | _ -> raise InvalidCommand
    end 
  | h::t -> begin match h with 
      | "help" -> Help (helphelper t)
      | "changename" -> ChangeName (namehelper t)
      | "play" -> Play (createCard t)
      | _ -> raise InvalidCommand
    end 

(** [readDif s] is the int representing the difficulty mode the player 
    selected. 
    The difficulties are Easy: 0, Medium: 1, Hard: 2, Supreme: 3. 
    Raises: [InvalidCommand] if [s] is not a valid difficulty. *)
let readDif s =
  let w = s |> String.split_on_char ' ' |> del_space [] |> List.rev in 
  if List.tl w = [] then
    begin match List.hd w with 
      | "easy" -> 0
      | "medium" -> 1
      | "hard" -> 2
      | "supreme" -> 3
      | _ -> raise InvalidCommand
    end 
  else raise InvalidCommand

(** [readMode s] is the int representing the game mode selected by the player. 
    The gamemode are Standard: 0, Stacking: 1, Draw Forever: 2, Dos: 3.
    Raises: [InvalidCommand] if [s] is not a valid game mode. *)
let readMode s =
  let w = s |> String.split_on_char ' ' |> del_space [] |> List.rev in 
  if List.tl w = [] then 
    begin match List.hd w with 
      | "standard" -> 0
      | "stacking" -> 1
      | "dos" -> 3
      | _ -> raise InvalidCommand
    end
  else if (List.length w = 2) && (List.nth w 0 = "draw") && 
          (List.nth w 1 = "forever") then 2
  else raise InvalidCommand

(** [readEnv s] is the player-chosen playing environment. The choices are
    "online" and "offline". 
    Raises: [InvalidCommand] if the player's input isn't a valid environment. *)
let readEnv s = 
  let w = s |> String.split_on_char ' ' |> del_space [] |> List.rev in 
  if List.tl w = [] then
    begin match List.hd w with 
      | "online" -> "online"
      | "offline" -> "offline"
      | _ -> raise InvalidCommand
    end
  else raise InvalidCommand

(** [readPort s] is the player entered port number for the server. 
    Raises: [InvalidCommand] if the port number is invalid. *)
let readPort s =
  let w = s |> String.split_on_char ' ' |> del_space [] |> List.rev in 
  match w with 
  | a::_ -> (try int_of_string a with (Failure _ | Invalid_argument _) -> 
      (raise InvalidCommand))
  | _ -> raise InvalidCommand

(** [readIp s] is the player-entered IP address for the server. 
    Raises: [InvalidCommand] if the IP address is invalid. *)
let readIp s =
  let w = s |> String.split_on_char ' ' |> del_space [] |> List.rev in 
  match w with 
  | a::_ -> 
    begin try 
        let temp = a |> String.split_on_char '.' |> List.map int_of_string in
        if (List.length temp <> 4) then (raise InvalidCommand) else a
      with (Failure _ | Invalid_argument _) -> 
        (raise InvalidCommand) 
    end 
  | _ -> raise InvalidCommand
