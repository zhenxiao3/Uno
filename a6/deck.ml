(** [Deck.ml] creates and reshuffles the Deck of UNO! cards. *)

(** [color] is the Type that represents the colors of a uno card. *)
type color = Green | Red | Blue | Yellow | Wild

(** [card] is the Type that represents a card of the uno deck. *)
type card = {
  color: color;
  value: int;
  card_type: string;
}

(** [deck] is the Type that represents a uno deck of cards. *)
type deck = card list

(** [OutofCards] is raised when the uno deck is empty and a card is attempted to 
    be drawn. *)
exception OutofCards of int*card list

(** [empty] is the empty deck of cards. *)
let empty = []

(** [is_empty d] is true if [d] is an empty uno deck, false otherwise. *)
let is_empty d = (d = empty)

(** [shuffle deck] is the uno deck [d'] after the [deck] has been shuffled
    while the top card of [deck] remains the top card of [d']. *)
let shuffle deck =
  match deck with
  | [] -> raise (OutofCards (0, [])) (* Shouldn't happen, since we don't shuffle 
                                        unless deck is out of cards. *)
  | h :: t -> 
    h::QCheck.Gen.(generate1 (shuffle_l t))

(** [gen_same_type_diff_color card_type i acc] is the list of cards with the
    same card_type [card_type] and value [i] but in all four colors,
    respectively, added onto the input list [acc]. *)
let gen_same_type_diff_color (cardtype : string) (i : int) (acc : deck) : deck =
  let green = {color = Green; value = i; card_type = cardtype} in
  let red = {green with color = Red} in
  let blue = {green with color = Blue} in
  let yellow = {green with color = Yellow} in
  green :: red :: blue :: yellow :: acc

(** [gen_num_cards i acc] is the list of NumberCard's numbered from 0-9 in all
    four colors: Green, Red, Blue, Yellow, respectively. *)
let rec gen_num_cards (i : int) (acc : deck) =
  if (i = 0) then (acc)
  else (gen_num_cards (i - 1) (gen_same_type_diff_color "NumberCard" i acc))

(** [shuffle_full deck] is the uno deck [d'] after the [deck] has been
    shuffled entirely. *)
let shuffle_full deck =
  QCheck.Gen.(generate1 (shuffle_l deck))

(** [check_init_top d] is the shuffled deck [d'] that has any card as its top
    card but the PlusFour card, since, according to the rules, the game must
    not begin with a PlusFour card. *)
let rec check_init_top (d : deck) = 
  let temp_top = List.hd d in
  let wild_card = {color = Wild; value = -1; card_type = "Wild"} in
  let plus4_card = {color = Wild; value = -2; card_type = "PlusFour"} in
  if (temp_top <> plus4_card && temp_top <> wild_card) 
  then d else (check_init_top (shuffle_full d))

(** [init_deck] returns the initial shuffled deck of 108 uno cards. *)
let init_deck = 
  let wild_card = {color = Wild; value = -1; card_type = "Wild"} in
  let plus4_card = {wild_card with card_type = "PlusFour"; value = -2} in
  let half_deck = wild_card :: wild_card :: plus4_card :: plus4_card :: []
                  |> gen_same_type_diff_color "Skip" 10 
                  |> gen_same_type_diff_color "PlusTwo" 11
                  |> gen_same_type_diff_color "Reverse" 12 
                  |> gen_num_cards 9 in
  half_deck@half_deck 
  |> gen_same_type_diff_color "NumberCard" 0 
  |> shuffle_full
  |> check_init_top

(** [draw i acc] is the tuple (c,d') where c is the list of the first i cards
    drawn from the uno deck and d' is the resulting uno deck after i cards are
    removed. 
    Raises: [OutofCards] if the deck is empty and cards still are to 
    be drawn. *)
let rec draw i (acc : card list) = function
  | [] -> if (i <> 0) then (raise (OutofCards (i, acc))) else (acc, empty)
  | h::t -> if (i = 0) then (acc, h :: t) else (draw (i - 1) (h :: acc) t)

(** [push card d] is the uno deck [d'] after [card] is added to the uno 
    deck [d]. *)
let push card d = card :: d

(** [peek d] is the top uno card of the deck [d] 
    Raises: [OutofCards] if deck is [empty] *)
let peek (deck : deck) : card = 
  match deck with
  | [] -> raise (OutofCards (0, [])) 
  (* should not happen, since we only peek 
     off of the discard pile, should never be out of cards. *)
  | h::t -> h