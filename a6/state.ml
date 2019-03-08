open Deck
open Unix

type player = {
  is_bot: bool;
  name: string;
  hand: Deck.card list;
  playable_cards: Deck.card list;
  sock: (file_descr*sockaddr);
  unod: bool;
  wonnered: bool;
}

type t = {
  discard_pile: Deck.deck;
  deck: Deck.deck;
  players: player list;
  turn: int;
  (* Standard = 0; Stacking = 1; Draw = 2; Supreme = 3*)
  mode: int;
  difficulty: int;
  stack : Deck.card list;
}

exception InvalidMove

exception PendingColor

exception GameOver

exception InvalidName

exception FalseUno

exception AutoDraw

(** [rename_player state name] is the state [t'] after the current player of
    [state] has changed their name to [name]
    Raises: [InvalidName] if the input [name] is already a name of a player in
    [state] *)
let rename_player (state : t) (name : string) = 
  let names = List.map (fun x -> x.name) state.players in
  if (List.mem name names) then 
    (* makes sure that all players have different names for distinction
       purposes *)
    (raise InvalidName) else
    let cur_player = List.nth state.players state.turn in
    let new_player = {cur_player with name = name} in
    let new_players = List.map (fun x -> if (x = cur_player) 
                                 then new_player else x) state.players in
    {state with players = new_players}

(** [pass state] is the state [t'] after the current player of state [state]
    passes the turn onto the next player. *)
let rec pass (state : t) = 
  let nturn = (state.turn + 1) mod (List.length state.players) in
  let stack_type = if (state.stack = []) then 20 
    else (List.hd state.stack).value in
  let potential_nstate = 
    {state with turn = nturn; 
                stack = (if (stack_type = 11) then state.stack else [])} in
  if (List.nth state.players nturn).wonnered then (pass potential_nstate) 
  else potential_nstate

(** [playable_cards top acc lst] is the list of cards that are allowed to be
      played given that [top] is the top card of the discard pile and [lst] is
      the current hand owned. 
      Example: returns empty list [] if no cards in [lst] are playable. *)
let rec playable_cards (top : Deck.card) (acc : Deck.card list) = function
  | [] -> acc
  | h::t ->
    let top_color = top.color in
    let top_num = top.value in
    if (top_color = h.color) then (playable_cards top (h::acc) t)
    else if (top_num = h.value && top_num <> -1 && top_num <> -2) then 
      (playable_cards top (h::acc) t) 
    else if (h.value = -1 || h.value = -2) then (playable_cards top (h::acc) t) 
    else (playable_cards top acc t)

(** [uno state] is the state [t'] after the current player of state calls 'uno'
    at state [state]. *)
let uno (state : t) = 
  let cur_player = List.nth state.players state.turn in
  if (List.length cur_player.hand <> 1) then state else
    let new_player = {cur_player with unod = true} in
    let new_players = List.map (fun x -> 
        if (x = cur_player) then new_player else x) state.players in
    {state with players = new_players}

(** [check_new_deck acc lst] is the deck of cards with wild and plusfour cards
    reverted back to their original form, which has color [Wild]. *)
let rec check_new_deck (acc : Deck.card list) = function
  | [] -> acc
  | h::t -> if (h.value = -1 || h.value = -2) 
    then (check_new_deck ({h with color = Wild}::acc) t) 
    else (check_new_deck (h::acc) t)

(** [force_draw_helper state target_player] is the new state [t'] after 
    [target_player] is forced to draw two cards. 
    Raises: [OutofCards (i,v)] if the deck is empty. *)
let force_draw_helper state target_player =
  let drawcards = Deck.draw 2 [] state.deck in
  let new_hand = target_player.hand @ fst drawcards in
  let updatedplayer = {
    is_bot = target_player.is_bot; name = target_player.name;
    hand = List.sort compare new_hand;
    playable_cards = playable_cards (Deck.peek state.discard_pile) [] new_hand; 
    sock = target_player.sock;
    unod = false; wonnered = false;} in
  {state with
   deck = snd (drawcards);
   players = List.map 
       (fun x -> if (x = target_player) then updatedplayer else x)
       state.players}

(** [force_draw state turn] is the new state [t'] after the player of index
    [turn] is forced to draw 2 cards *)
let force_draw (state : t) (turn : int) : t = 
  let target_player = List.nth state.players turn in
  try (force_draw_helper state target_player)
  with (OutofCards (i, v)) -> (
      let topl, shuffledeck = Deck.draw 1 [] 
          (Deck.shuffle state.discard_pile) in
      let newdeck = Deck.draw i [] shuffledeck in
      let new_hand = target_player.hand @ v @ fst newdeck in
      let updatedplayer = {
        is_bot = target_player.is_bot; name = target_player.name;
        hand = new_hand; sock = target_player.sock;
        playable_cards = playable_cards (Deck.peek state.discard_pile) [] 
            new_hand;
        unod = false; wonnered = false} in 
      let new_discard_pile = topl in
      {state with
       discard_pile = new_discard_pile;
       deck = List.rev (check_new_deck [] (snd newdeck));
       players = List.map 
           (fun x -> if (x = target_player) then updatedplayer else x)
           state.players})

(** [uno_on_others state] is the new state [t'] after the current player of
    [state] attempts to call uno on other players
    Raises: [FalseUno] if no other player has only one card left *)
let uno_on_others (state : t) : t =
  let rec index_of elt counter = function
    | [] -> failwith "Not happening in this context"
    | h::t -> (if h = elt then counter else index_of elt (counter + 1) t) in
  let cur_player = List.nth state.players state.turn in
  let rec helper acc = function
    | [] -> acc
    | h::t -> 
      if ((List.length h.hand) = 1 && (h <> cur_player) && (not h.unod))
      then (helper ((index_of h 0 state.players)::acc) t) 
      else (helper acc t) in
  let one_carders = helper [] state.players in
  if (List.length one_carders = 0) then (raise FalseUno) 
  else (
    let rec apply_uno state = function
      | [] -> state
      | h::t -> apply_uno (force_draw state h) t in
    apply_uno state one_carders)

(** [update_playables_helper top acc lst] is the list of players in reversed
    order with updated playable_cards given the new top card of the discard
    deck [top]. *)
let rec update_playables_helper (top : Deck.card) (acc : player list) = function
  | [] -> acc
  | h::t -> let cur_player = 
              {h with playable_cards = List.sort compare 
                          (playable_cards top [] h.hand)} in
    update_playables_helper top (cur_player::acc) t

(** [update_playables top state] is the list of players with updated
    playable_cards given the new top card of the discard deck [top] at state
    [state]. *)
let update_playables (top : Deck.card) (state : t) = 
  List.rev (update_playables_helper top [] state.players)

(** [generate_players is_bot top deck num acc names] is the tuple of the list of
    [num] number of players generated and the residule deck after distrubuting
    cards for the players from deck [deck]. *)
let rec generate_players (is_bot : bool) (top : Deck.card) (deck : Deck.deck) 
    (num : int) (acc : player list) (names : string list) = 
  let hand, new_deck = Deck.draw 7 [] deck in
  if (num = 0) then (acc, deck) else generate_players true top
      new_deck (num - 1) ({
          hand = List.sort compare hand;
          is_bot = is_bot;
          name = (List.nth names (num - 1));
          sock = (stderr, (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
          unod = false;
          playable_cards = List.sort compare (playable_cards top [] hand);
          wonnered = false}::acc) names

(** [init_game num_players] is the state of a new uno game with [num_players]
    amount of players with randomly generated hands of 7.
    Requires: [num_players] is between 2 and 4.
    Note: single player game would have [num_player] of 2. *)
let init_game (num_players : int) (mode : int) (diff : int) : t = 
  Random.self_init ();
  let top, new_deck = Deck.draw 1 [] Deck.init_deck in
  let after_gen_players = generate_players false (List.hd top) 
      new_deck num_players [] (Jsonparser.generate_names num_players) in
  let temp_st =
    {discard_pile = top; deck = snd after_gen_players;
     players = fst after_gen_players; turn = 0; mode = mode;
     difficulty = diff; stack = []} in
  {temp_st with players = update_playables (List.hd top) temp_st}

(** [draw1helper state cards oldplayer] is the state [t'] after [oldplayer] has 
    drawn [cards] out of the current deck. 
    Raises: [OutofCards (i,v)] if the deck is out of cards. *)
let draw1helper state cards oldplayer =
  let drawcards = Deck.draw cards [] state.deck in
  let new_hand = oldplayer.hand @ (check_new_deck [] (fst drawcards)) in
  let updatedplayer = {
    is_bot = oldplayer.is_bot; name = oldplayer.name;
    hand = List.sort compare new_hand; sock = oldplayer.sock;
    playable_cards = playable_cards (Deck.peek state.discard_pile) [] 
        new_hand; unod = false; wonnered = false} in
  if cards = 1 then 
    {discard_pile = state.discard_pile; deck = snd (drawcards);
     players = List.mapi (fun ind play -> if (ind = state.turn) 
                           then (updatedplayer) else (play)) state.players; 
     turn = state.turn; mode = state.mode;
     stack = state.stack; difficulty = state.difficulty}
  else 
    {discard_pile = state.discard_pile; deck = snd (drawcards);
     players = List.mapi (fun ind play -> if (ind = state.turn) 
                           then (updatedplayer) else (play)) state.players;
     turn = (state.turn + 1) mod List.length (state.players);
     mode = state.mode; stack = state.stack; difficulty = state.difficulty}

(** [draw1 state cards] is the state [t'] after the current player has drawn
    [cards] out of the current deck.
    If the current deck runs out of cards, the deck discard_pile is shuffled
    and becomes the new deck. 
    The remaining cards are drawn from the new pile. *)
let draw1 (state : t) (cards : int) : t =
  let oldplayer = (List.nth state.players state.turn) in
  try (draw1helper state cards oldplayer)
  with (OutofCards (i, v)) -> (
      let topl, shuffledeck = Deck.draw 1 [] 
          (Deck.shuffle state.discard_pile) in
      let newdeck = Deck.draw i [] shuffledeck in
      let new_hand = check_new_deck [] (oldplayer.hand @ v @ fst newdeck) in
      let updatedplayer = {
        is_bot = oldplayer.is_bot; name = oldplayer.name; hand = new_hand;
        playable_cards = playable_cards (Deck.peek state.discard_pile) [] 
            new_hand; unod = false; wonnered = false; sock = oldplayer.sock} in 
      let new_discard_pile = topl in
      {discard_pile = new_discard_pile;
       deck = List.rev (check_new_deck [] (snd newdeck));
       players = List.mapi (fun ind play -> if (ind = state.turn) 
                             then (updatedplayer) else (play)) state.players;
       turn = (state.turn + 1) mod List.length (state.players);
       mode = state.mode; stack = state.stack; difficulty = state.difficulty})

(** [stack_draw state] is the state after the current player attempts to draw
    cards. If the stack of the state is [empty], the current player draws
    normally. But if the stack is full of +2's, the current player fails to
    draw and has the stack of +2's added to his/her hand.
    Requires: game mode is 1 (Stacking). *)
let stack_draw (state : t) (cards : int) : t =
  let stack = state.stack in
  let tp = if (stack = []) then 20 else (List.hd stack).value in
  if (tp = 11) 
  then (let temp_st = draw1 state (2 * (List.length stack)) in
        {temp_st with stack = []; 
                      discard_pile = temp_st.stack @ temp_st.discard_pile})
  else (draw1 state cards)

(** [draw state cards] is the state [t'] after the current player draws [cards]
    number of cards at [state]
    Applicable for all modes. *)
let draw (state : t) (cards : int) : t =
  if (state.mode = 0 || state.mode = 2) then draw1 state cards
  else stack_draw state cards

(** [cheatdraw1helper state cards oldplayer] is the state [t'] after 
    supreme ai [oldplayer] has drawn [cards] out of the current deck. 
    Raises: [OutofCards (i,v)] if the deck is out of cards. *)
let cheatdraw1helper (state: t) (cards : int) (oldplayer) : t =
  let drawcards = Deck.draw cards [] state.deck in
  let rand = Random.int 2 in 
  let wild_card = {color = Wild; value= -1; card_type = "Wild"} in
  let plus4_card = {wild_card with card_type = "PlusFour"; value = -2} in
  let new_hand = 
    fst drawcards |> List.tl
    |> List.cons (if (rand = 0) then wild_card else plus4_card)
    |> check_new_deck [] |> List.append oldplayer.hand in 
  let updatedplayer = 
    {is_bot = oldplayer.is_bot; name = oldplayer.name;
     hand = List.sort compare new_hand; sock = oldplayer.sock;
     playable_cards = playable_cards (Deck.peek state.discard_pile) [] new_hand; 
     unod = false; wonnered = false} in
  if cards = 1 then 
    {discard_pile = state.discard_pile; deck = snd (drawcards);
     players = List.mapi (fun ind play -> if (ind = state.turn) 
                           then (updatedplayer) else (play)) state.players; 
     turn = state.turn; mode = state.mode;
     stack = state.stack; difficulty = state.difficulty}
  else 
    {discard_pile = state.discard_pile; deck = snd (drawcards);
     players = List.mapi (fun ind play -> if (ind = state.turn) 
                           then (updatedplayer) else (play)) state.players;
     turn = (state.turn + 1) mod List.length (state.players);
     mode = state.mode; stack = state.stack; difficulty = state.difficulty}

(** [draw1 state cards] is the state [t'] after the current supreme AI has drawn
    [cards] out of the current deck.
    If the current deck runs out of cards, the deck discard_pile is shuffled
    and becomes the new deck. 
    The remaining cards are drawn from the new pile. *)
let cheatdraw1 (state : t) (cards : int) : t =
  let oldplayer = (List.nth state.players state.turn) in
  try (cheatdraw1helper state cards oldplayer)
  with (OutofCards (i, v)) -> (
      let topl, shuffledeck = Deck.draw 1 [] 
          (Deck.shuffle state.discard_pile) in
      let newdeck = Deck.draw i [] shuffledeck in
      let new_hand = check_new_deck [] (oldplayer.hand @ v @ fst newdeck) in
      let updatedplayer = 
        {is_bot = oldplayer.is_bot; name = oldplayer.name; hand = new_hand;
         playable_cards = playable_cards (Deck.peek state.discard_pile) [] 
             new_hand; unod = false; wonnered = false; sock = oldplayer.sock} in 
      let new_discard_pile = topl in
      {discard_pile = new_discard_pile;
       deck = List.rev (check_new_deck [] (snd newdeck));
       players = List.mapi (fun ind play -> if (ind = state.turn) 
                             then (updatedplayer) else (play)) state.players;
       turn = (state.turn + 1) mod List.length (state.players);
       mode = state.mode; stack = state.stack; difficulty = state.difficulty})

(** [cheat_stack_draw state] is the state after the supreme AI attempts to draw
    cards. If the stack of the state is [empty], the supreme AI draws
    normally. But if the stack is full of +2's, the supreme AI fails to
    draw and has the stack of +2's added to his/her hand.
    Requires: game mode is 1 (Stacking). *)
let cheat_stack_draw (state : t) (cards : int) : t =
  let stack = state.stack in
  let tp = if (stack = []) then 20 else (List.hd stack).value in
  if (tp = 11) 
  then (let temp_st = cheatdraw1 state (2 * (List.length stack)) in
        {temp_st with stack = []; 
                      discard_pile = temp_st.stack @ temp_st.discard_pile})
  else (cheatdraw1 state cards)

(** [cheatdraw] is the state [t'] after the current supreme bot player draws
    [cards] *)
let cheatdraw (state : t) (cards : int) : t =
  if (state.mode = 0 || state.mode = 2) then cheatdraw1 state cards
  else cheat_stack_draw state cards

(** [check_valid state card] checks if the card [card] is playable at state
    [state]. 
    Returns: [card] if it a valid playable card according to the rules of UNO!
    Raises: [InvalidMove] if [card] is not playable. *)
let check_valid (state : t) (card : Deck.card) =
  let cur_hand = (List.nth state.players state.turn).playable_cards in
  if (List.mem card cur_hand) then (card) else (raise InvalidMove)

(** [check_uno state player] checks if the [player] called uno when he/she 
    has only one card left. If he/she did, returns the current [state]. 
    Otherwise, the [player] has to draw 2 cards. *)
let check_uno (state : t) (player : player) =
  if (List.length player.hand = 1 && player.unod) || 
     (List.length player.hand <> 1)
  then state
  else (let new_st = draw state 2 in
        let p = List.nth new_st.players new_st.turn in
        let new_players = (List.map (fun v -> 
            if (v = p) then {p with unod = false; wonnered = false} 
            else v) new_st.players) in
        {new_st with players = new_players})

(** [custom_find acc found card color lst] is the list of cards after assigning
    the wild card [card] a color [color]. *)
let rec custom_find (acc : Deck.card list) (found : bool) (card : Deck.card) 
    (color:Deck.color) = function
  | [] -> acc
  | h::t -> if found then (custom_find (h::acc) true card color t) 
    else if (h = card) 
    then (custom_find ({card with color = color}::acc) true card color t) 
    else (custom_find (h::acc) false card color t)

(** [change_wildcard_color state card color] is the state of the game
    after the Wild or PlusFour [card] the current player holds has been 
    redefined by the input color in [state]. *)
let change_wildcard_color (state : t) (card : Deck.card) 
    (color : Deck.color) : t =
  let cur_player = List.nth state.players state.turn in
  let new_player = 
    {cur_player with hand = custom_find [] false card color cur_player.hand} in
  let new_players = (List.map (fun v -> 
      if (v = (List.nth state.players state.turn)) 
      then new_player else v) state.players) in
  let fin_players = update_playables (Deck.peek state.discard_pile) 
      {state with players = new_players} in
  {state with players = fin_players}

(** [change_color state color] is the state [t'] after changing the color of 
    the top-most card in the discard pile of state [state] to color [color]
    Raises: [Invalid_argument] if the input color [color] is [Wild]. *)
let change_color (state : t) (color : color) : t =
  if (color = Wild) 
  then (raise (Invalid_argument "Color Wild is not playable")) 
  else
    let top, popped_deck = Deck.draw 1 [] state.discard_pile in
    let new_deck = {(List.hd top) with color = color}::popped_deck in
    {state with discard_pile = new_deck}

(** [play1helper st_before new_top state] is the state [t'] after the 
    player plays a speciality card. *)
let play1helper st_before new_top state =
  match new_top.card_type with 
  | "Skip" -> {st_before with turn = (st_before.turn + 1) mod 
                                     (List.length st_before.players)}
  | "PlusTwo" -> 
    {(draw st_before 2) with 
     turn = (st_before.turn + 1) mod (List.length st_before.players)} 
  | "PlusFour" -> 
    change_color {(draw st_before 4) with 
                  turn = (st_before.turn + 1)
                         mod (List.length st_before.players)} new_top.color
  | "Reverse" -> 
    (let new_player_lst = List.rev st_before.players in
     let new_turn = (((List.length st_before.players) - state.turn - 1) + 1) mod 
                    (List.length st_before.players) in
     {st_before with players= new_player_lst; turn= new_turn}) 
  | "Wild" -> (change_color st_before new_top.color) 
  | _ -> st_before

(** [play1 state card] is the state [t'] after the current player of state 
    [state] attempts to play card [card] in game mode 0 (Standard).
    Raises: [InvalidMove] if [card] is not playable by the current player of
    state [state]. *)
let play1 (state : t) (card : Deck.card) = 
  let cur_player = List.nth state.players state.turn in
  let cur_hand = cur_player.hand in
  if (cur_player.wonnered) then (pass state) 
  else let new_st = check_uno state cur_player in
    let new_top_card = check_valid new_st card in
    let new_state = 
      {new_st with players = update_playables new_top_card new_st} in
    let new_discard_pile = new_top_card :: new_state.discard_pile in
    let nh = List.sort compare (List.filter (fun v -> v <> card) cur_hand) in
    let new_hand = (if List.length nh = (List.length cur_hand)-1 then nh else 
                      List.sort compare (card::nh)) in
    let new_person = 
      {(List.nth new_state.players new_state.turn) 
       with hand = new_hand;
            playable_cards = playable_cards new_top_card [] new_hand;
            wonnered = if ((List.length new_hand) = 0) then true else false} in
    let st_b = {
      new_state with players = (List.map (fun v -> 
        if (v = (List.nth new_state.players new_state.turn)) 
        then new_person else v) new_state.players);
           discard_pile = new_discard_pile;
           turn = (state.turn + 1) mod (List.length state.players)} in
    let new_top = Deck.peek st_b.discard_pile in
    let st_before = {st_b with players = update_playables new_top st_b} in
    (* Below are implementations of the functionality of special cards *)
    play1helper st_before new_top state

(** [pseudo_play state card] is the state [t'] after the player pushes the
    [card] onto the stack of cards in [state]
    Raises: [InvalidMove] if the player does not hold the [card]
    Requires: [card] has type of either [PlusTwo] or [NumberCard] *)
let pseudo_play (state : t) (card : Deck.card) cur_player : t =
  if (cur_player.wonnered) then (pass state) 
  else let cur_hand = cur_player.hand in
    let new_st = check_uno state cur_player in
    let new_top_card = check_valid new_st card in
    let new_state = 
      {new_st with players = update_playables new_top_card new_st} in
    let new_discard_pile = new_top_card :: new_state.discard_pile in
    let nh = List.sort compare (List.filter (fun v -> v <> card) cur_hand) in
    let new_hand = (if List.length nh = (List.length cur_hand)-1 then nh 
                    else List.sort compare (card::nh)) in
    let new_person = {
      (List.nth new_state.players new_state.turn) 
      with hand = new_hand;
           playable_cards = playable_cards new_top_card [] new_hand;
           wonnered = if ((List.length new_hand) = 0) then true else false} in
    {new_state with players = (List.map (fun v -> 
         if (v = (List.nth new_state.players new_state.turn)) 
         then new_person else v) new_state.players);
            discard_pile = new_discard_pile;
            stack = (card :: new_state.stack); turn = state.turn}

exception TakeStack

(** [playhelper state card stack_type cur_player] is the state [t'] after the 
    current player tries to play [card], where [card] is not a number card or 
    drawtwo. *)
let playhelper state card stack_type cur_player = 
  (* if stack is empty, and player does not play either +2 or number card *)
  if (stack_type = 20) then (play1 state card)
  else (
    (* case when +2's have been stacked and current player does not
        continue stacking +2's, so he/she has to take the stack. *)
    if (stack_type = 11) then (
      if (cur_player.is_bot) then raise TakeStack
      else
        (let temp_st = 
           draw state (2 * (List.length state.stack)) in
         {temp_st with stack = []; 
                       turn = (state.turn + 1) mod (List.length state.players);
                       discard_pile = temp_st.stack @ temp_st.discard_pile})) 
    else (raise InvalidMove))

(** [play state card] is the state [t'] after the player of [state] attempts
    to play [card] *)
let play (state : t) (card : Deck.card) : t =
  (* command [play] does not change for modes Standard and Draw *)
  let cur_player = List.nth state.players state.turn in
  let stack_type = if (state.stack = []) then 20 else 
      (List.hd state.stack).value in
  if (state.mode = 0 || state.mode = 2) then (play1 state card)
  (* command [play] allows stacking 2's, and stacking number cards *)
  else (* Note to self: [stack] needs to be checked every time play is called; 
           "check" entails if [stack] is full of (+2)'s, cur_player needs to
           draw that many cards; if not, player can play any card that has the 
           same number and add to the stack, turn does not change. We do not 
           allow stacking of other cards *)
    (* the player isn't allowed to continue stacking. *)
  if (card.value < 0 || card.value = 10 || card.value = 12) 
  then (playhelper state card stack_type cur_player) 
  (* the player may want to continue stacking. *)
  else if card.value = stack_type || stack_type = 20 
  then (pseudo_play state card cur_player)
  else if (stack_type = 11) 
  then (if (cur_player.is_bot) then raise TakeStack else
          (let temp_st = draw state (2 * (List.length state.stack)) in
           {temp_st with 
            stack = []; 
            turn = (state.turn + 1) mod (List.length state.players);
            discard_pile = temp_st.stack @ temp_st.discard_pile}))
  else (raise InvalidMove)

(** [auto_play_helper state] is the state [t'] after the current player (an AI 
    of difficulty medium) plays. 
    Raises: [InvalidMove] if the AI attempts an illegal move. 
    Raises: [Invalid_argument] if one of the arguments is invalid. *)
let auto_play_helper state =
  let cur_player = List.nth state.players state.turn in
  let cur_playable_hand = cur_player.playable_cards in
  let rand_card = List.nth cur_playable_hand 
      (Random.int (List.length cur_playable_hand)) in
  let st = 
    if (List.length cur_player.hand = 1) then uno state else state in
  (* Generate a random color to pass through the wild cards *)
  let rand_color_ind = Random.int 4 in
  let rand_color = begin match rand_color_ind with 
    | 0 -> Green | 1 -> Red | 2 -> Blue | _ -> Yellow end in 
  let wild_card = {color = Wild; value= -1 ; card_type = "Wild"} in
  let plus4_card = {wild_card with card_type = "PlusFour"; value = -2} in
  ((* If the random card chosen is not a wild card or a plus 4, (both
       require an input of color), play it as a normal card *)
    match rand_card.card_type with 
    | "Wild" -> play (change_wildcard_color st wild_card rand_color) 
                  {wild_card with color = rand_color}
    | "PlusFour" -> play (change_wildcard_color st plus4_card rand_color) 
                      {plus4_card with color = rand_color}
    | _ -> if (state.mode = 0 || state.mode = 2) then play st rand_card
      else if (rand_card.value = 10 || rand_card.value = 12)
      then play st rand_card else pass (play st rand_card))

(** [auto_play state] is the state [t'] after the current player of [state]
    plays, assuming the current player of [state] is a 
    difficulty medium computer AI. 
    Raises: [AutoDraw] if the AI has no playable cards. 
    Raises: [TakeStack] if the AI is forced to take the stack of plustwos. *)
let auto_play (state : t) = 
  try auto_play_helper state
  with (InvalidMove | Invalid_argument _) -> 
    (let stack_type = if (state.stack = []) then 20 
       else (List.hd state.stack).value in
     if (stack_type = 11) then (raise TakeStack) else
       raise AutoDraw)

(* Helper Method for betterautoplay *)
(** [check_next_cards] is the number of cards the next player has *)
let check_next_cards (state : t) : int =
  List.length (List.nth state.players 
                 ((state.turn + 1) mod (List.length state.players))).hand 

(* Helper Method for betterautoplay *)
(** [findcolor] is the sublist of cards [lst'], of [lst] where each 
    element of [lst'] has color [color] *)
let rec findcolor (lst : Deck.card list) (color : Deck.color) acc =
  match lst with 
  | [] -> acc
  | h::t -> if (h.color = color) then (findcolor t color (h::acc))
    else findcolor t color acc

(* Helper Method for betterautoplay *)
(** [intersect] is the intersection, [lst'] of the two lists [lst1], [lst2]
    In other words, every element of [lst'] is both an elemtn of [lst1] and an
    element of [lst2]. *)
let intersect lst1 lst2 = 
  let ls1 = List.sort compare lst1 in 
  let ls2 = List.sort compare lst2 in 
  let rec intersecthelp l1 l2 =
    match l1 with 
    | [] -> []
    | h1::t1 ->
      begin 
        match l2 with 
        | [] -> []
        | h2::t2 when h1 < h2 -> intersecthelp t1 l2
        | h2::t2 when h1 > h2 -> intersecthelp l1 t2
        | h2::t2 -> h1::(intersecthelp t1 t2)
      end in 
  intersecthelp ls1 ls2 

(* Helper Method for betterautoplay *)
(** [colorhelper] is the tuple (length, [lst']) where lst' is the sublist of 
    [lst] such that every element of [lst'] is color [color] and length
    is the length of this sublist. *)
let colorhelper (lst : Deck.card list) (color : Deck.color) = 
  let temp = (findcolor lst color []) in
  (List.length temp, temp)

(* Helper Method for betterautoplay *)
(** [intersection] is the list of playable cards of the greatest color in the 
    list [sortedcolors] *)
let rec intersection (lst2 : Deck.card list) = function
  | [] -> []
  | h::t when (intersect (snd h) lst2 = []) -> intersection lst2 t
  | h::t -> intersect (snd h) lst2 

(* Helper Method for betterautoplay *)
(** [findpluscards acc lst] is the sublist [list'] of [lst] where each element 
    of [list'] is of type [PlusTwo] or [PlusFour] *)
let rec findpluscards acc = function 
  | [] -> acc
  | h::t when h.card_type = "PlusTwo" || h.card_type = "PlusFour" 
    -> findpluscards (h::acc) t
  | h::t -> findpluscards acc t 

(* Helper Method for Supreme autoplay *)
(** [findwildcards acc lst] is the sublist [list'] of [lst] where each element 
    of [list'] is of type [Wild] *)
let rec findwildcards acc = function  
  | [] -> acc
  | h::t when h.card_type = "Wild" -> findwildcards (h::acc) t
  | h::t -> findwildcards acc t

(** [colorFinder state cur_hand nm_hand] is the color the AI should change 
    playing color to when the AI plays a Wild. *)
let colorFinder state cur_hand nm_hand =
  (* Find color with the most number of cards *)
  let reds = (colorhelper cur_hand Deck.Red)::[] in
  let blues = (colorhelper cur_hand Deck.Blue)::reds in 
  let greens = (colorhelper cur_hand Deck.Green)::blues in
  let yellows = (colorhelper cur_hand Deck.Yellow)::greens in
  let sortedcolors = List.rev (List.sort compare yellows) in 
  (* Finds enemy's weakest colors *)
  let nmreds = (colorhelper nm_hand Deck.Red)::[] in
  let nmblues = (colorhelper nm_hand Deck.Blue)::nmreds in 
  let nmgreens = (colorhelper nm_hand Deck.Green)::nmblues in
  let nmyellows = (colorhelper nm_hand Deck.Yellow)::nmgreens in
  let nmsortedcolors = (List.sort compare nmyellows) in 
  let nmweakest = 
    if (fst (List.hd nmsortedcolors) = fst (List.hd nmreds)) then Red 
    else if (fst (List.hd nmsortedcolors) = fst (List.hd nmblues)) then Blue 
    else if (fst (List.hd nmsortedcolors) = fst (List.hd nmgreens)) then Green
    else Yellow in nmweakest, sortedcolors

(** [playNormally state] is the state [t'] after the difficult AI plays,
    assuming that the AI doesn't need to play a special card due to the
    circumstances. 
    Raises: [InvalidMove] if the move the AI attempts is against the rules. 
    Raises: [Invalid_argument] if the inputs are incorrect. 
    Raises: [AutoDraw] if the AI has no playable cards. *)
let playNormally st sortedcolors cur_playable_hand cur_hand majoritycolor
    wild_card plus4_card =
  let playcard = List.rev (intersection cur_playable_hand sortedcolors) in 
  (* If you can play a regular card, then play it*)
  if not (playcard = []) then 
    (if (st.mode = 0 || st.mode = 2) then play st (List.hd playcard)
     else if ((List.hd playcard).card_type <> "NumberCard"
              && (List.hd playcard).card_type <> "PlusTwo") 
     then (play st (List.hd playcard)) 
     else pass (play st (List.hd playcard)))
  else (
    (* If you don't have a regular card, 
       play a wild card w/ color majority*)
    let wilds = List.sort compare (findcolor cur_hand Deck.Wild []) in 
    if (wilds = []) then raise AutoDraw 
    else if ((List.hd wilds).card_type = "Wild") 
    then (play (change_wildcard_color st wild_card majoritycolor) 
            {wild_card with color = majoritycolor}) 
    else (play (change_wildcard_color st plus4_card majoritycolor) 
            {plus4_card with color = majoritycolor}))

(** [tryBetterAutoplay state] is the state [t'] after the current player 
    (an AI in difficult mode) attempts to play a card. 
    Raises: [InvalidMove] if the AI isn't allowed to play that card. 
    Raises: [Invalid_argument] if a function's precondition is violated. *)
let tryBetterAutoplay state = 
  (let cur_player = List.nth state.players state.turn in
   let cur_hand = cur_player.hand in 
   let cur_playable_hand = cur_player.playable_cards in
   let st = if (List.length cur_hand = 1) then uno state else state in
   let pluscards = findpluscards [] cur_hand in
   let plbpluscards = intersect pluscards cur_playable_hand in 
   let nm_hand = (List.nth state.players 
                    (state.turn mod (List.length state.players))).hand in 
   let wild_card = {color = Wild; value= -1; card_type = "Wild"} in
   let plus4_card = {wild_card with card_type = "PlusFour"; value = -2} in
   let nmweakest, sortedcolors = colorFinder state cur_hand nm_hand in 
   (*Majority color is your highest color or opponenets lowest*)
   let majoritycolor = if (fst (List.hd sortedcolors) = 0) then nmweakest 
     else (List.hd (snd (List.hd sortedcolors))).color in 
   if (check_next_cards st <= 3 && not (plbpluscards = [])) 
   then 
     (if ((List.hd plbpluscards).card_type = "PlusTwo") 
      then (if (state.mode = 0 || state.mode = 2)
            then (play st (List.hd plbpluscards))
            else (pass (play st (List.hd plbpluscards))))
      else (play (change_wildcard_color st plus4_card majoritycolor) 
              {plus4_card with color = majoritycolor}))     
   else (playNormally st sortedcolors cur_playable_hand cur_hand 
           majoritycolor wild_card plus4_card))

(** [betterautoplay] is the state [t'] after the current player of state
    [state] plays, assuming the current player of [state] is 
    difficult computer AI. 
    The AI functions by analyzing its the next opponent's hand and in the event
    that the next player is about to win and the AI has a playable plus card,
    it will play it. Furthermore, the AI plays with a heursitic, ranking
    each card by its inherent point value. Thus, in the event that the AI
    loses, the AI will lose with fewer points. The heurstic is done as follows:
    Skips, PlusTwos, and Reverses are worth compartively large number of points 
    in spite of having color limitations on their play. Hence the AI plays these
    cards decidely first. On the otherhand, PlusFours and Wild cards are also 
    worth large numbers of points but can be played at any time. Hence the AI
    decides to play these cards when an opponent has fewer than 3 cards left. 
    Otherwise, the AI plays a playable number card in the order from 9 to 0.
    In the event that the AI does not have any playable color cards, the AI will
    play a wild card of the color it has the greatest of. *)
let betterautoplay (state : t) =
  try tryBetterAutoplay state 
  with (InvalidMove | Invalid_argument _) -> (
      let stack_type = if (state.stack = []) then 20 
        else (List.hd state.stack).value in
      if (stack_type = 11) then (raise TakeStack) else raise AutoDraw)

(** [cheatingAttack state] is the state after the bot plays an attack card,
    given that the bot is a Supreme AI. *)
let cheatingAttack state plbpluscards st plus4_card majoritycolor=
  if ((List.hd plbpluscards).card_type = "PlusTwo") 
  then (if (state.mode = 0 || state.mode = 2)
        then (play st (List.hd plbpluscards))
        else (pass (play st (List.hd plbpluscards))))
  else (play (change_wildcard_color st plus4_card majoritycolor) 
          {plus4_card with color = majoritycolor})

(** [tryCheatingAutoplay state] is the state after the Supreme AI attempts
    to play a card. 
    Raises: [InvalidMove] if the bot attempts to play a card that violates the
    rules of UNO!
    Raises: [Invalid_argument] if a function's precondition is violated. *)
let tryCheatingAutoplay state =
  let cur_player = List.nth state.players state.turn in
  let cur_hand = cur_player.hand in 
  let cur_playable_hand = cur_player.playable_cards in
  let st = 
    if (List.length cur_player.hand = 1) then uno state else state in
  let pluscards = findpluscards [] cur_hand in
  let wildcards = findwildcards [] cur_hand in 
  let plbpluscards = intersect pluscards cur_playable_hand in
  let plbwildcards = intersect wildcards cur_playable_hand in 
  let nm_player = List.nth state.players 
      ((state.turn + 1) mod (List.length state.players)) in 
  let nm_hand = nm_player.hand in
  let wild_card = {color = Wild; value= -1; card_type = "Wild"} in
  let plus4_card = {wild_card with card_type = "PlusFour"; value = -2} in
  let nmweakest, sortedcolors = colorFinder state cur_hand nm_hand in 
  (*Majority color is your highest color or opponenets lowest*)
  let majoritycolor = 
    if (fst (List.hd sortedcolors) = 0) then nmweakest 
    else (List.hd (snd (List.hd sortedcolors))).color in 
  if (check_next_cards st <= 4 && not (plbpluscards = [])) 
  then cheatingAttack state plbpluscards st plus4_card majoritycolor
  else if (check_next_cards st <= 4 && not (plbwildcards = []))
  then play (change_wildcard_color st wild_card nmweakest) 
      {wild_card with color = nmweakest}              
  else (playNormally st sortedcolors cur_playable_hand cur_hand 
          majoritycolor wild_card plus4_card)

(** [cheatingautoplay state] is the state [t'] after the supreme AI plays
    a card. *)
let cheatingautoplay (state : t) = 
  try tryCheatingAutoplay state 
  with (InvalidMove | Invalid_argument _) -> (
      let stack_type = if (state.stack = []) then 20 
        else (List.hd state.stack).value in
      if (stack_type = 11) then (raise TakeStack) else raise AutoDraw)

(** [worseautoplay] is the state [t'] after the current player of state
    [state] plays, assuming the current player of [state] is 
    easy computer AI. *)
let worseautoplay (state : t) = 
  try
    (let randomdraw = Random.int 4 in 
     if randomdraw = 0 then (raise AutoDraw) else
       let randomuno = Random.int 4 in 
       let cur_player = List.nth state.players state.turn in
       let cur_hand = cur_player.hand in
       if (List.length cur_player.hand = 1 && randomuno = 0 
           && List.length cur_player.playable_cards = 1) then 
         play state (List.hd cur_hand) else auto_play state)
  with (InvalidMove | Invalid_argument _) -> (
      let stack_type = if (state.stack = []) then 20 
        else (List.hd state.stack).value in
      if (stack_type = 11) then (raise TakeStack) else
        raise AutoDraw)

(** [auto_play_wild state] is the new state [t'] after the computer AI player
    chooses to respond to the [Wild] card on top of the discard pile *)
let auto_play_wild (state : t) =
  let rand_color_ind = Random.int 4 in
  let rand_color = match rand_color_ind with 
    | 0 -> Green 
    | 1 -> Red 
    | 2 -> Blue 
    | _ -> Yellow in 
  change_color state rand_color

(* Note: if the initial top card is a number card, it has no
   effect to the first player. The initial top card cannot be a
   PlusFour card, as specified in the rules *)
(** [init_top_effect state] is the state [t'] after the effect of the initial
    top card of the deck has affected the first player.
    Raises: [PendingColor] if the top card is a [Wild] card, and the first
    player gets to choose the color of the initial game. *)
let init_top_effect (state : t) = 
  let top = Deck.peek state.discard_pile in
  begin match top.card_type with 
    | "Skip" -> {state with turn = (state.turn + 1) mod 
                                   (List.length state.players)}
    | "PlusTwo" -> {(draw state 2) with 
                    turn = (state.turn + 1) mod (List.length state.players)}
    | "Wild" -> raise PendingColor
    | "Reverse" -> let new_player_lst = List.rev state.players in
      let new_turn = ((List.length state.players) - state.turn - 1) mod 
                     (List.length state.players) in
      {state with players= new_player_lst; turn= new_turn}
    | _ -> state
  end 

(** [gsow_helper lst] finds the winner in the player list [lst]. Returns [None]
    if there is no winner in [lst], and [Some w] if the winner is found *)
let rec gsow_helper = function
  | [] -> None
  | h :: t -> if (h.wonnered) then Some h else gsow_helper t

(** [game_status_one_winner state] is [true] if the human player of the game
    wins, and [false] if there are no winners yet, and the game continues.
    Raises: [GameOver] if there is a winner, and it is not the human player.
    Requires: the game has only one human player and any number of computer AI
    players *)
let game_status_one_winner (state : t) : bool =
  let winner = gsow_helper state.players in
  begin
    match winner with
    | None -> false
    | Some w -> if (w.is_bot) then (raise GameOver) else true
  end

(** [get_winner_points state] is the calculated points of the winner of the
    game of [state]. When a player wins, they receive points. All opponents'
    cards are given to the winner and points are counted. *)
let get_winner_points (state : t) : int =
  let rec helper acc = function
    | [] -> acc
    | h :: t -> if (not h.wonnered) then (helper (h :: acc) t)
      else (helper acc t) in
  let losers = helper [] state.players in
  let hands = List.map (fun x -> x.hand) losers in
  let rec count_hand acc = function
    | [] -> acc
    | h :: t -> let temp = h.value in
      (* if it's a number card, point value is just its card value *)
      if (temp >= 0 && temp <= 9) then count_hand (temp + acc) t
      (* if it's a plustwo, reverse, or skip card, 20 points *)
      else if (temp > 9) then count_hand (20 + acc) t 
      (* if it's a wild or plusfour, 50 points *)
      else count_hand (50 + acc) t in
  List.fold_left (+) 0 (List.map (count_hand 0) hands)

(** [get_winner_name state] is the name of the winner. *)
let get_winner_name (state : t) : string =
  let winnerL = List.filter (fun a -> a.wonnered = true) state.players in
  let winner = List.nth winnerL 0 in 
  winner.name
