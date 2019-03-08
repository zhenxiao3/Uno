(** Type that represents the colors of cards in a uno deck. *)
type color = Green | Red | Blue | Yellow | Wild

(** Type that represents the possible cards in a uno deck. *)
type card = {
  color: color;
  value: int;
  card_type: string;
}

(** Type that represents a deck of uno cards. *)
type deck = card list

(** [OutofCards] is raised when a player attempts to draw from an empty deck. *)
exception OutofCards of int*card list

(** [empty] is the empty uno deck *)
val empty: deck

(** [is_empty d] is true if [d] is [empty], and false otherwise. *)
val is_empty: deck -> bool

(** [init_deck] is the initial shuffled deck of uno cards when a 
    game is initialzed. *)
val init_deck: deck

(** [draw i d] is the tuple (h,d') where h is the list of the first [i] cards 
    drawn in the deck [d] and [d'] is the new deck after [i] cards are drawn.
    Raises: [OutofCards] if i > 0 and deck is empty *)
val draw: int -> card list -> deck -> (card list) * deck

(** [push c d] is the discard deck after the card [c] has been played, pushing the
    card to the top of the discard deck [d] *)
val push: card -> deck -> deck

(** [shuffle d] is [d'] after [d] has been shuffled.
    Returns: [empty] if [d] is [empty] *)
val shuffle: deck -> deck

(** [peek d] is the top card of the deck [d] 
    Raises: [OutofCards] if d is empty *)
val peek: deck -> card 