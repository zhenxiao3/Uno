(** State.mli *)
(** type [player] represents a player of the game Uno. *)
type player = {
  is_bot: bool;
  name: string;
  hand: Deck.card list;
  playable_cards: Deck.card list;
  sock: (Unix.file_descr*Unix.sockaddr);
  unod: bool;
  wonnered: bool
}

(** type [t] represents the current game state. *)
type t = {
  discard_pile: Deck.deck;
  deck: Deck.deck;
  players: player list;
  turn: int;
  mode: int;
  difficulty: int;
  stack: Deck.card list;
}

(** [InvalidMove] is the exception that represents an attempted move by the
    player that is against the rules of uno. *)
exception InvalidMove

(** [PendingColor] is the exception that represents an activated [Wild] card
    without a designated color. *)
exception PendingColor

(** [GameOver] is the exception that represents the end of a game when the
    computer AI wins over the human player. *)
exception GameOver

(** [AutoDraw] is the exception that represents the situation when the computer
    AI player has no cards to play and has to draw. *)
exception AutoDraw

(** [InvalidName] is the exception that represents an invalid name the player
    attempts to input when renaming themselves in the game. *)
exception InvalidName

(** [FalseUno] is the exception that represents the situation when the player
    calls 'Uno' but no one in the game has one card left. *)
exception FalseUno

(** [TakeStack] is the exception that represents the situation when the
    computer AI player takes the toll of the entire stack of PlusTwo's in mode
    Stacking *)
exception TakeStack

(** [init_game i m] initializes a game of [i] players with state [t']. State
    [t'] contains a shuffled [deck] of 108 cards, with its top card extracted
    and put in the [discard_pile]. [t'] also contains the list of players, and
    [turn] is initialized to [0], the first player in the list. Finally, the
    game mode is initialized to [m], where Standard = 0; Stacking = 1; and
    Draw = 2.
    Example: a "single player" game can be initialized by [init_game 2]. *)
val init_game: int -> int -> int -> t

(** [draw st i] is the state [t'] after the current player of state [st] draws
    [i] number of cards from the deck at state [st]. *)
val draw: t -> int -> t

(** [cheatdraw st i] is the state [t'] after the current supreme bot of state
    [st] draws [i] number of cards from the deck at state [st]. *)
val cheatdraw: t -> int -> t

(** [play st card] is the state [t'] after the current human player of state
    [st] attempts to play card [card].
    Raises: [InvalidMove] if [card] is not playable at state [st]. *)
val play: t -> Deck.card -> t

(** [change_color state color] is the state [t'] after changing the color of 
    the top-most card in the discard pile of state [state] to color [color].
    This function is used when the top card has initial color [Wild], and the
    current player defines a color, that is not [Wild], for the top card.
    Raises: [Invalid_argument] if the input color [color] is [Wild]. *)
val change_color: t -> Deck.color -> t

(** [change_wildcard_color state card color] is the state after the wild card
    or plus four card the current player holds has been redefined by the input
    color [color] in state [state]. This function should be called in main
    every time the player input is either [Wild] or [PlusFour], and its output
    stored as the new state of the game.
    Requires: the current player of state [state] has a card of [Wild] color in
    his/her hand. *)
val change_wildcard_color: t -> Deck.card -> Deck.color -> t

(** [auto_play st] is the [play] function for medium difficulty computer
    AI players. *)
val auto_play: t -> t

(** [betterautoplay st] is the [play] function for hard difficulty computer
    AI players. *)
val betterautoplay: t -> t

(** [worseautoplay st] is the [play] function for easy difficulty computer
    AI players. *)
val worseautoplay: t -> t

(** [cheatingautoplay st] is the [play] function for supreme difficulty computer
    AI players. *)
val cheatingautoplay: t-> t

(** [auto_play_wild st] is the special case when the computer AI player is the
    first player to play in the game and the initial top card is a [Wild] card.
    This is the new state [t'] after the AI randomly chooses a color to
    respond to the inital [Wild] card in the game represented by state [st]. *)
val auto_play_wild: t -> t

(** [init_top_effect state] is the state [t'] after the effect of the initial
    top card of the deck has been taken by the first player.
    Raises: [PendingColor] if the top card is a [Wild] card, and the first
    player gets to choose the color of the initial game. *)
val init_top_effect: t -> t

(** [game_status_one_winner state] is [true] if the human player of the game has
    won, [false] if there are no winners and the game continues.
    Raises: [GameOver] if a computer AI wins *)
val game_status_one_winner: t -> bool

(** [pass st] is the updated state after the current player of state [st]
    passes his/her turn onto the next player in line. *)
val pass: t -> t

(** [uno st] is the updated state after the current player of state [st] calls
    'uno'. *)
val uno: t -> t

(** [uno_on_others state] is the updated state after the current player of
    [state] attempts to call uno on other players
    Raises: [FalseUno] if no other player has only one card left *)
val uno_on_others: t -> t

(** [rename_player st name] is the updated state after the current player of
    state [st] changes their name to [name] *)
val rename_player: t -> string -> t

(** [get_winner_points st] is the number of points the winner recieve after
    they win the game of [st]. When a player wins, they receive points. All
    opponents' cards go to the winner, and the winner's points are calculated
    as the following: a NumberCard has points equivalent to its card value;
    a DrawTwo, Reverse, or skip has 20 points; and a Wild or PlusFour has
    50 points. *)
val get_winner_points: t -> int

(** [get_winner_name st] is either the computer generated or the player chosen
    name of the player who won the game of uno. *)
val get_winner_name: t -> string 
