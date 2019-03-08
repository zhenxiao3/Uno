(** Parse.mli 
    Parse is responsible for all the reading and writing to and from files as 
    well as translating user input into usable commands. *)

(** Type to represent the possible game commands. *)
type command = 
  | Quit
  | Draw
  | Uno
  | Play of Deck.card 
  | Pass
  | Rules of string 
  | Help of string 
  | ChangeName of string 

(** Exception for commands that aren't valid. *)
exception InvalidCommand

(** [read s] is the uno command form of the player's input. 
    Raises: [InvalidCommand] if the player's input isn't a valid uno command. *)
val read: string -> command 

(** [cardColor s] is the color of the card the player plays. 
    Raises: [InvalidCommand] if the color isn't a legal card color in uno. *)
val cardColor: string list -> Deck.color

(** [highScore] is a string list of player names and scores from the 
    Leaderboard. As the leaderboard only contains the top 10 scores, the 
    length of [highscore] will be less than or equal to 10. 
    [highScore] may be the empty list. *)
val highScore: string list

(** [updateHighScores lst] saves the new leaderboard after a round of uno has
    been played. [lst] is the new list of high scores with the user's score 
    potentially included if it is high enough. *)
val updateHighScores: string list -> unit

(** [getCommands sep] is the list of commands in the game, seperated by 
    [sep]. *)
val getCommands: string -> string 

(** [readDif s] is the player-chosen game difficulty, valid difficulties are
    ['Easy'], ['Medium'] and ['Hard']. 
    The difficulties are not case sensitive.
    Raises: [InvalidCommand] if the player's input is not a valid difficulty
    level. *)
val readDif: string -> int 

(** [readMode s] is the player-chosen game mode. The valid game modes are 
    ['Standard'], ['Stacking'], and ['Draw Forever'].
    The modes are not case sensitive. 
    Raises: [InvalidCommand] is the player's input is not a valid game mode. *)
val readMode: string -> int

(** [readEng s] is the player-chosen game environment. The valid environments
    are ['Online'] and ['Offline'].
    The environments are not case sensitive. 
    Raises: [InvalidCommand] is the player's input is not a valid environment.*)
val readEnv: string -> string

(** [readPort s] is the port number of the server. The valid port numbers are
    commonly 4-digit to 5-digit integers.
    Raises: [InvalidCommand] is the player's input is not a valid port number.*)
val readPort: string -> int

(** [readIp s] is the IP address of the server. The valid IP addresses are four
    integers separated by '.' with no extra characters in between.
    Raises: [InvalidCommand] is the player's input is not a valid IP address. *)
val readIp: string -> string

(** [displayCard] is the art displayed for the player pulled from file [f]. *)
val displayCard: string -> string