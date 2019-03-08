open OUnit2
open Unix


let make_read_test 
    (name : string)
    (input : string)
    (output : Parse.command) : test =
  name >:: (fun _ -> assert_equal output (Parse.read input))

let make_read_error_test
    (name : string)
    (input : string) : test =
  name >:: (fun _ -> assert_raises Parse.InvalidCommand 
               (fun _ -> Parse.read input))

let make_readDif_test
    (name : string)
    (input : string)
    (output : int) : test =
  name >:: (fun _ -> assert_equal output (Parse.readDif input))

let make_readMode_test
    (name : string)
    (input : string)
    (output : int) : test =
  name >:: (fun _ -> assert_equal output (Parse.readMode input))

let make_readDif_error_test
    (name : string)
    (input : string) : test =
  name >:: (fun _ -> assert_raises Parse.InvalidCommand 
               (fun _ -> Parse.readDif input))

let make_readMode_error_test
    (name : string)
    (input : string) : test =
  name >:: (fun _ -> assert_raises Parse.InvalidCommand 
               (fun _ -> Parse.readMode input))


(* Due to the ever changing nature of our Rules, Commands, and Score files,
   it is impractical to test any function that has to read from those files
   by comparing the output of that function to a static value. However, those
   functions were extensively tested by running the game, and shown off in
   our demo. 
   Additionally, the helper cardColor is thoroughly tested by the read tests. 
   It is only a public function so that it can also be used in Main to 
   avoid repeating code. *)


let parse_tests =
  [
    make_readDif_test "easy test" "easy" 0;
    make_readDif_test "medium test" "medium" 1;
    make_readDif_test "hard test" "hard" 2;
    make_readDif_test "easy in caps" "EASY" 0;
    make_readDif_test "medium with spaces" "    medium    " 1;
    make_readDif_test "hard with caps and spaces" "   HaRd   " 2; 
    make_readDif_error_test "easy with other words" "easy game";
    make_readDif_error_test "medium with other words" "medium mode! ldsv";
    make_readDif_error_test "hard with other words" "HARD modoooode jdhv";
    make_readDif_error_test "completely invalid word" "sdhvshdvksdvsdvsdv";
    make_readMode_test "standard test" "standard" 0;
    make_readMode_test "stacking test" "stacking" 1;
    make_readMode_test "draw forever test" "draw forever" 2;
    make_readMode_test "standard with caps" "STANDARD" 0;
    make_readMode_test "stacking with spaces" "  stacking   " 1;
    make_readMode_test "draw forever with caps and spaces" " DRaw    FORevER" 2;
    make_readMode_error_test "standard with extra words" "standard   sdv";
    make_readMode_error_test "stacking with extra words" "stacking sidh sdh";
    make_readMode_error_test "draw forever with extra words" "draw forever d f";
    make_readMode_error_test "completely incorrect input" "dfhhsdf sdj";
    make_read_test "draw test" "draw" Parse.Draw;
    make_read_test "uno test" "uno" Parse.Uno;
    make_read_test "pass test" "pass" Parse.Pass; 
    make_read_test "play number card" "play blue 7" 
      (Parse.Play {color=Deck.Blue; value=7; card_type="NumberCard"});
    make_read_test "play skip card" "play red skip" 
      (Parse.Play {color=Deck.Red; value=10; card_type="Skip"});
    make_read_test "play reverse card" "play yellow reverse" 
      (Parse.Play {color=Deck.Yellow; value= 12; card_type= "Reverse"});
    make_read_test "play plustwo" "play green plustwo" 
      (Parse.Play {color=Deck.Green; value=11; card_type= "PlusTwo"});
    make_read_test "play wild" "play wild red"
      (Parse.Play {color=Deck.Red; value= -1; card_type="Wild"});
    make_read_test "play plusfour" "play plusfour blue" 
      (Parse.Play {color=Deck.Blue; value= -2; card_type="PlusFour"});
    make_read_test "play Wild (caps)" "PlAy WilD yEllow"
      (Parse.Play {color=Deck.Yellow; value= -1; card_type="Wild"});
    make_read_test "play PlusFour (caps)" "plaY PLUSFOUR greEn"
      (Parse.Play {color=Deck.Green; value= -2; card_type = "PlusFour"});
    make_read_test "play numbercard Caps" "PLAY GREEN 0"
      (Parse.Play {color=Deck.Green; value=0; card_type="NumberCard"});
    make_read_test "lots of spaces" "     draw    " Parse.Draw;
    make_read_test "spaces around play" "   Play  blue     skip   "
      (Parse.Play {color=Deck.Blue; value=10; card_type="Skip"});
    make_read_test "changename test" "changename x" (ChangeName "X");
    make_read_test "changename multi-word name" "changename sir pass please" 
      (ChangeName "Sir Pass Please");
    make_read_test "help with no second word" "help" 
      (Help (Parse.getCommands "\n"));
    make_read_error_test "changename with no name" "changename";
    make_read_error_test "help with invalid command" "help meee";
    make_read_error_test "invalid command word" "sidvh green 7";
    make_read_error_test "invalid color" "play osdih 9";
    make_read_error_test "invalid number" "play green 46";
    make_read_error_test "no input" "";
    make_read_error_test "only spaces" "     ";
    make_read_error_test "draw with multiple words" "draw cards";
    make_read_error_test "uno with more words" "uno yay! ";
    make_read_error_test "play with no card name" "play";
    make_read_error_test "pass with extra words" "pass the game";
    make_read_error_test "play with too many words" "play blue green 5 6";
    make_read_error_test "play with too few words" "play card"
  ]

let make_draw_test 
    (name : string)
    (state : State.t)
    (cards : int)
    (output : State.t) : test =
  name >:: (fun _ -> assert_equal output (State.draw state cards))

let make_draw_shuffled_test
    (name : string)
    (state : State.t)
    (cards : int)
    (output : int) : test = 
  name >:: (fun _ -> 
      assert_equal output (List.length (State.draw state cards).deck))

let make_play_test 
    (name : string)
    (state : State.t)
    (card : Deck.card)
    (output : State.t) : test =
  name >:: (fun _ -> assert_equal output (State.play state card))

let make_play_ex_stack_test
    (name : string)
    (state : State.t)
    (card : Deck.card) : test =
  name >:: (fun _ -> assert_raises (State.TakeStack)
               (fun _ -> State.play state card))

let make_play_exception_test
    (name : string)
    (state : State.t)
    (card : Deck.card) : test =
  name >:: (fun _ -> assert_raises (State.InvalidMove) 
               (fun _ -> State.play state card))

let make_change_color_test 
    (name : string)
    (state : State.t)
    (color : Deck.color)
    (output : State.t) : test =
  name >:: (fun _ -> assert_equal output (State.change_color state color))

let make_change_wildcard_color_test 
    (name : string)
    (state : State.t)
    (card : Deck.card)
    (color : Deck.color)
    (output : State.t) : test =
  name >:: (fun _ -> assert_equal output 
               (State.change_wildcard_color state card color))

let make_init_top_effect_test 
    (name : string)
    (state : State.t)
    (output : State.t) : test =
  name >:: (fun _ -> assert_equal output (State.init_top_effect state))

let make_itet_exception_test
    (name : string)
    (state : State.t) : test =
  name >:: (fun _ -> assert_raises (State.PendingColor) 
               (fun _ -> State.init_top_effect state))

let make_gsow_test 
    (name : string)
    (state : State.t)
    (output : bool) : test =
  name >:: (fun _ -> assert_equal output 
               (State.game_status_one_winner state))

let make_gsow_exception_test
    (name : string)
    (state : State.t) : test =
  name >:: (fun _ -> assert_raises (State.GameOver) 
               (fun _ -> State.game_status_one_winner state))

let make_rename_test
    (name : string)
    (state : State.t)
    (nname : string)
    (output : State.t) : test =
  name >:: (fun _ -> assert_equal output (State.rename_player state nname))

let make_winner_pts_test
    (name : string)
    (state : State.t)
    (output : int) : test =
  name >:: (fun _ -> assert_equal output (State.get_winner_points state))

let make_winner_name_test
    (name : string)
    (state : State.t)
    (output : string) : test =
  name >:: (fun _ -> assert_equal output (State.get_winner_name state))

let make_pass_test
    (name : string)
    (state : State.t)
    (output: State.t) : test =
  name >:: (fun _ -> assert_equal output (State.pass state))

let make_uno_test
    (name : string)
    (state : State.t)
    (output: State.t) : test =
  name >:: (fun _ -> assert_equal output (State.uno state))

let make_emptydeck_test
    (name : string)
    (output: Deck.card list) : test =
  name >:: (fun _ -> assert_equal output ([]))

let make_deck_isempty_test
    (name: string)
    (deck: Deck.deck)
    (expected_output: bool ) : test =
  name >:: (fun _ -> assert_equal expected_output (Deck.is_empty deck))

let make_deckdraw_test
    (name : string)
    (i : int)
    (acc : Deck.card list)
    (deck : Deck.deck)
    (output: Deck.card list*Deck.deck) : test =
  name >:: (fun _ -> assert_equal output (Deck.draw i acc deck))

let make_push_test
    (name : string)
    (card : Deck.card)
    (deck : Deck.deck)
    (output: Deck.card list) : test =
  name >:: (fun _ -> assert_equal output (Deck.push card deck))

let make_peek_test 
    (name : string)
    (deck : Deck.deck)
    (output: Deck.card) : test =
  name >:: (fun _ -> assert_equal output (Deck.peek deck))

let make_init_test 
    (name : string)
    (output: int) : test =
  name >:: (fun _ -> assert_equal (List.length Deck.init_deck) (output))

(* states made for [State.pass] and [State.uno] tests *)
let bt_st : State.t = {
  discard_pile = []; deck = [];
  players = [{is_bot = false; 
              name = "lol";
              hand = [{color = Yellow; value = 4; card_type = "NumberCard"}];
              playable_cards = []; unod = false;
              sock = (stderr,
                      (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
              wonnered = false};
             {is_bot = true; hand = []; playable_cards = []; unod = false;
              sock = (stderr,
                      (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
              wonnered = false; name = "lol2"}];
  turn = 0; mode = 0; stack = []; difficulty = 0
}
let pass_st : State.t = {bt_st with turn = 1}
let uno_st : State.t = {bt_st with players = [
    {is_bot = false; name = "lol";
     hand = [{color = Yellow; value = 4; card_type = "NumberCard"}];
     playable_cards = []; unod = true;
     sock = (stderr,
             (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
     wonnered = false};
    {is_bot = true; hand = []; playable_cards = []; unod = false;
     sock = (stderr,
             (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
     wonnered = false; name = "lol2"}
  ]}

(* states made for [State.draw] tests *)
let before1_draw : State.t = {
  discard_pile = [{color = Red; value = 3; card_type = "NumberCard"}];
  deck = [{color = Yellow; value = 4; card_type = "NumberCard"};
          {color = Wild; value = -1; card_type = "Wild"}];
  players = [{is_bot = false; hand = []; playable_cards = []; unod = false;
              sock = (stderr,
                      (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
              wonnered = false; name = "lol"}]; turn = 0; mode = 0;
  stack = []; difficulty = 0}
let after1_draw_2 : State.t = 
  {State.discard_pile = [{color = Red; value = 3; card_type = "NumberCard"}];
   deck = [];
   players =
     [{State.is_bot = false; name = "lol";
       hand =
         [{color = Yellow; value = 4; card_type = "NumberCard"};
          {color = Wild; value = -1; card_type = "Wild"}];
       playable_cards = [{color = Wild; value = -1; card_type = "Wild"}];
       sock = (stderr,
               (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
       unod = false; wonnered = false}];
   turn = 0; mode = 0; stack = []; difficulty = 0}
let before2_draw : State.t = {
  before1_draw with 
  deck = [];
  discard_pile = [{color = Red; value = 3; card_type = "NumberCard"};
                  {color = Blue; value = 3; card_type = "NumberCard"};
                  {color = Green; value = 3; card_type = "NumberCard"};
                  {color = Yellow; value = 3; card_type = "NumberCard"}];}

(* states made for [State.play] tests. [State.auto_play] is not tested due to
    randomization *)
let before_play : State.t = {
  discard_pile = [{color = Red; value = 3; card_type = "NumberCard"};];
  deck =
    [{color = Blue; value = 3; card_type = "NumberCard"};
     {color = Yellow; value = 3; card_type = "NumberCard"};
     {color = Green; value = 3; card_type = "NumberCard"};
     {color = Red; value = 3; card_type = "NumberCard"}];
  players =
    [{is_bot = false; name = "lol";
      hand = [{color = Green; value = 3; card_type = "NumberCard"};
              {color = Red; value = 8; card_type = "NumberCard"};
              {color = Blue; value = 4; card_type = "NumberCard"};
              (* assuming that main has taken care of the change of color of
                 Wild cards *)
              {color = Blue; value = -1; card_type = "Wild"};
              {color = Blue; value = -2; card_type = "PlusFour"};
              {color = Red; value = 12; card_type = "Reverse"};
              {color = Red; value = 10; card_type = "Skip"};
              {color = Red; value = 11; card_type = "PlusTwo"}
             ];
      playable_cards = [{color = Green; value = 3; card_type = "NumberCard"};
                        {color = Red; value = 8; card_type = "NumberCard"};
                        {color = Blue; value = -1; card_type = "Wild"};
                        {color = Blue; value = -2; card_type = "PlusFour"};
                        {color = Red; value = 12; card_type = "Reverse"};
                        {color = Red; value = 10; card_type = "Skip"};
                        {color = Red; value = 11; card_type = "PlusTwo"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false};
     {is_bot = true; name = "lol2";
      hand = [{color = Green; value = 3; card_type = "NumberCard"}];
      playable_cards = [{color = Green; value = 3; card_type = "NumberCard"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false}]; turn = 0; mode = 0; 
  stack = []; difficulty = 0}

let after_play_green3 = {
  before_play with
  discard_pile = [{color = Green; value = 3; card_type = "NumberCard"};
                  {color = Red; value = 3; card_type = "NumberCard"};];
  players = [
    {is_bot = false; name = "lol";
     hand = [{color = Red; value = 8; card_type = "NumberCard"};
             {color = Red; value = 10; card_type = "Skip"};
             {color = Red; value = 11; card_type = "PlusTwo"};
             {color = Red; value = 12; card_type = "Reverse"};
             {color = Blue; value = -2; card_type = "PlusFour"};
             {color = Blue; value = -1; card_type = "Wild"};
             {color = Blue; value = 4; card_type = "NumberCard"}];
     playable_cards = [{color = Blue; value = -2; card_type = "PlusFour"};
                       {color = Blue; value = -1; card_type = "Wild"}];
     sock = (stderr,
             (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
     unod = false; wonnered = false};
    {is_bot = true; name = "lol2";
     hand = [{color = Green; value = 3; card_type = "NumberCard"}];
     playable_cards = [{color = Green; value = 3; card_type = "NumberCard"}];
     sock = (stderr,
             (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
     unod = false; wonnered = false}
  ]; turn = 1;}

let after_play_red8 = 
  {State.discard_pile =
     [{color = Red; value = 8; card_type = "NumberCard"};
      {color = Red; value = 3; card_type = "NumberCard"}];
   deck =
     [{color = Blue; value = 3; card_type = "NumberCard"};
      {color = Yellow; value = 3; card_type = "NumberCard"};
      {color = Green; value = 3; card_type = "NumberCard"};
      {color = Red; value = 3; card_type = "NumberCard"}];
   players =
     [{State.is_bot = false; name = "lol";
       hand =
         [{color = Green; value = 3; card_type = "NumberCard"};
          {color = Red; value = 10; card_type = "Skip"};
          {color = Red; value = 11; card_type = "PlusTwo"};
          {color = Red; value = 12; card_type = "Reverse"};
          {color = Blue; value = -2; card_type = "PlusFour"};
          {color = Blue; value = -1; card_type = "Wild"};
          {color = Blue; value = 4; card_type = "NumberCard"}];
       playable_cards =
         [{color = Red; value = 10; card_type = "Skip"};
          {color = Red; value = 11; card_type = "PlusTwo"};
          {color = Red; value = 12; card_type = "Reverse"};
          {color = Blue; value = -2; card_type = "PlusFour"};
          {color = Blue; value = -1; card_type = "Wild"}];
       sock = (stderr,
               (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
       unod = false; wonnered = false};
      {State.is_bot = true; name = "lol2";
       hand = [{color = Green; value = 3; card_type = "NumberCard"}];
       sock = (stderr,
               (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
       playable_cards = []; unod = false; wonnered = false}];
   turn = 1; mode = 0; stack = []; difficulty = 0}

let after_play_bluewild = 
  {State.discard_pile =
     [{color = Blue; value = -1; card_type = "Wild"};
      {color = Red; value = 3; card_type = "NumberCard"}];
   deck =
     [{color = Blue; value = 3; card_type = "NumberCard"};
      {color = Yellow; value = 3; card_type = "NumberCard"};
      {color = Green; value = 3; card_type = "NumberCard"};
      {color = Red; value = 3; card_type = "NumberCard"}];
   players =
     [{State.is_bot = false; name = "lol";
       hand =
         [{color = Green; value = 3; card_type = "NumberCard"};
          {color = Red; value = 8; card_type = "NumberCard"};
          {color = Red; value = 10; card_type = "Skip"};
          {color = Red; value = 11; card_type = "PlusTwo"};
          {color = Red; value = 12; card_type = "Reverse"};
          {color = Blue; value = -2; card_type = "PlusFour"};
          {color = Blue; value = 4; card_type = "NumberCard"}];
       playable_cards =
         [{color = Blue; value = -2; card_type = "PlusFour"};
          {color = Blue; value = 4; card_type = "NumberCard"}];
       sock = (stderr,
               (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
       unod = false; wonnered = false};
      {State.is_bot = true; name = "lol2";
       hand = [{color = Green; value = 3; card_type = "NumberCard"}];
       sock = (stderr,
               (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
       playable_cards = []; unod = false; wonnered = false}];
   turn = 1; mode = 0; stack = []; difficulty = 0}

let after_play_blueplusfour = 
  {State.discard_pile =
     [{color = Blue; value = -2; card_type = "PlusFour"};
      {color = Red; value = 3; card_type = "NumberCard"}];
   deck = [];
   players =
     [{State.is_bot = false; name = "lol";
       hand =
         [{color = Green; value = 3; card_type = "NumberCard"};
          {color = Red; value = 8; card_type = "NumberCard"};
          {color = Red; value = 10; card_type = "Skip"};
          {color = Red; value = 11; card_type = "PlusTwo"};
          {color = Red; value = 12; card_type = "Reverse"};
          {color = Blue; value = -1; card_type = "Wild"};
          {color = Blue; value = 4; card_type = "NumberCard"}];
       playable_cards =
         [{color = Blue; value = -1; card_type = "Wild"};
          {color = Blue; value = 4; card_type = "NumberCard"}];
       sock = (stderr,
               (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
       unod = false; wonnered = false};
      {State.is_bot = true; name = "lol2";
       hand =
         [{color = Green; value = 3; card_type = "NumberCard"};
          {color = Green; value = 3; card_type = "NumberCard"};
          {color = Red; value = 3; card_type = "NumberCard"};
          {color = Blue; value = 3; card_type = "NumberCard"};
          {color = Yellow; value = 3; card_type = "NumberCard"}];
       playable_cards = [{color = Blue; value = 3; card_type = "NumberCard"}];
       sock = (stderr,
               (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
       unod = false; wonnered = false}];
   turn = 0; mode = 0; stack = []; difficulty = 0}

let after_play_redreverse = 
  {State.discard_pile =
     [{color = Red; value = 12; card_type = "Reverse"};
      {color = Red; value = 3; card_type = "NumberCard"}];
   deck =
     [{color = Blue; value = 3; card_type = "NumberCard"};
      {color = Yellow; value = 3; card_type = "NumberCard"};
      {color = Green; value = 3; card_type = "NumberCard"};
      {color = Red; value = 3; card_type = "NumberCard"}];
   players =
     [{State.is_bot = true; name = "lol2";
       hand = [{color = Green; value = 3; card_type = "NumberCard"}];
       sock = (stderr,
               (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
       playable_cards = []; unod = false; wonnered = false};
      {State.is_bot = false; name = "lol";
       hand =
         [{color = Green; value = 3; card_type = "NumberCard"};
          {color = Red; value = 8; card_type = "NumberCard"};
          {color = Red; value = 10; card_type = "Skip"};
          {color = Red; value = 11; card_type = "PlusTwo"};
          {color = Blue; value = -2; card_type = "PlusFour"};
          {color = Blue; value = -1; card_type = "Wild"};
          {color = Blue; value = 4; card_type = "NumberCard"}];
       playable_cards =
         [{color = Red; value = 8; card_type = "NumberCard"};
          {color = Red; value = 10; card_type = "Skip"};
          {color = Red; value = 11; card_type = "PlusTwo"};
          {color = Blue; value = -2; card_type = "PlusFour"};
          {color = Blue; value = -1; card_type = "Wild"}];
       sock = (stderr,
               (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
       unod = false; wonnered = false}];
   turn = 0; mode = 0; stack = []; difficulty = 0}

let after_play_redskip = {
  before_play with
  discard_pile = [{color = Red; value = 10; card_type = "Skip"};
                  {color = Red; value = 3; card_type = "NumberCard"};];
  players = [
    {is_bot = false; name = "lol";
     hand = [{color = Green; value = 3; card_type = "NumberCard"};
             {color = Red; value = 8; card_type = "NumberCard"};
             {color = Red; value = 11; card_type = "PlusTwo"};
             {color = Red; value = 12; card_type = "Reverse"};
             {color = Blue; value = -2; card_type = "PlusFour"};
             {color = Blue; value = -1; card_type = "Wild"};
             {color = Blue; value = 4; card_type = "NumberCard"}];
     playable_cards = [{color = Red; value = 8; card_type = "NumberCard"};
                       {color = Red; value = 11; card_type = "PlusTwo"};
                       {color = Red; value = 12; card_type = "Reverse"};
                       {color = Blue; value = -2; card_type = "PlusFour"};
                       {color = Blue; value = -1; card_type = "Wild"}];
     sock = (stderr,
             (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
     unod = false; wonnered = false};
    {is_bot = true; name = "lol2";
     hand = [{color = Green; value = 3; card_type = "NumberCard"}];
     sock = (stderr,
             (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
     playable_cards = []; unod = false; wonnered = false}]}
let after_play_redplustwo = 
  {State.discard_pile =
     [{Deck.color = Deck.Red; value = 11; card_type = "PlusTwo"};
      {Deck.color = Deck.Red; value = 3; card_type = "NumberCard"}];
   deck =
     [{Deck.color = Deck.Green; value = 3; card_type = "NumberCard"};
      {Deck.color = Deck.Red; value = 3; card_type = "NumberCard"}];
   players =
     [{State.is_bot = false; name = "lol";
       hand =
         [{Deck.color = Deck.Green; value = 3; card_type = "NumberCard"};
          {Deck.color = Deck.Red; value = 8; card_type = "NumberCard"};
          {Deck.color = Deck.Red; value = 10; card_type = "Skip"};
          {Deck.color = Deck.Red; value = 12; card_type = "Reverse"};
          {Deck.color = Deck.Blue; value = -2; card_type = "PlusFour"};
          {Deck.color = Deck.Blue; value = -1; card_type = "Wild"};
          {Deck.color = Deck.Blue; value = 4; card_type = "NumberCard"}];
       playable_cards =
         [{Deck.color = Deck.Red; value = 8; card_type = "NumberCard"};
          {Deck.color = Deck.Red; value = 10; card_type = "Skip"};
          {Deck.color = Deck.Red; value = 12; card_type = "Reverse"};
          {Deck.color = Deck.Blue; value = -2; card_type = "PlusFour"};
          {Deck.color = Deck.Blue; value = -1; card_type = "Wild"}];
       sock = (stderr,
               (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
       unod = false; wonnered = false};
      {State.is_bot = true; name = "lol2";
       hand =
         [{Deck.color = Deck.Green; value = 3; card_type = "NumberCard"};
          {Deck.color = Deck.Blue; value = 3; card_type = "NumberCard"};
          {Deck.color = Deck.Yellow; value = 3; card_type = "NumberCard"}];
       sock = (stderr,
               (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
       playable_cards = []; unod = false; wonnered = false}];
   turn = 0; mode = 0; stack = []; difficulty = 0}

(* states made for [State.change_color] tests *)
let unchanged_wild_st : State.t = {
  discard_pile = [{color = Wild; value = -1; card_type = "Wild"}]; deck = [];
  players = []; turn = 0; mode = 0; stack = []; difficulty = 0}
let changed_wild_st : State.t = {
  unchanged_wild_st with discard_pile = 
                           [{color = Blue; value = -1; card_type = "Wild"}]}

(* states made for [State.change_wildcard_color] tests *)
let unchanged_player_with_wild_st : State.t = {
  discard_pile = [{color = Red; value = 10; card_type = "Skip"}]; deck = [];
  players = [{
      is_bot = false; name = "lol";
      hand = [{color = Red; value = 12; card_type = "Reverse"};
              {color = Wild; value = -1; card_type = "Wild"};
              {color = Red; value = 8; card_type = "NumberCard"}];
      playable_cards = [{color = Red; value = 12; card_type = "Reverse"};
                        {color = Wild; value = -1; card_type = "Wild"};
                        {color = Red; value = 8; card_type = "NumberCard"}]; 
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false}];
  turn = 0; mode = 0; stack = []; difficulty = 0}
let changed_player_with_wild_st : State.t = {
  unchanged_player_with_wild_st with
  players = [{
      is_bot = false; name = "lol";
      hand = [{color = Red; value = 8; card_type = "NumberCard"};
              {color = Green; value = -1; card_type = "Wild"};
              {color = Red; value = 12; card_type = "Reverse"}];
      playable_cards = [{color = Green; value = -1; card_type = "Wild"};
                        {color = Red; value = 8; card_type = "NumberCard"};
                        {color = Red; value = 12; card_type = "Reverse"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false}]}
let player_wo_wild_st : State.t = {
  unchanged_player_with_wild_st with
  players = [{
      is_bot = false; name = "lol";
      hand = [{color = Red; value = 8; card_type = "NumberCard"};
              {color = Red; value = 12; card_type = "Reverse"}];

      playable_cards = [{color = Red; value = 12; card_type = "Reverse"};
                        {color = Red; value = 8; card_type = "NumberCard"}]; 
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false}]}
let player_wo_wild_st2 : State.t = {
  unchanged_player_with_wild_st with
  players = [{
      is_bot = false; name = "lol";
      hand = [{color = Red; value = 12; card_type = "Reverse"};
              {color = Red; value = 8; card_type = "NumberCard"}];
      playable_cards = [{color = Red; value = 8; card_type = "NumberCard"};
                        {color = Red; value = 12; card_type = "Reverse"}]; 
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false}]}

(* states made for [State.init_top_effect] tests *)
let before_skip : State.t = {
  discard_pile = [{color = Red; value = 10; card_type = "Skip"}];
  deck = [{color = Green; value = 3; card_type = "NumberCard"};
          {color = Red; value = 8; card_type = "NumberCard"};
          {color = Blue; value = 4; card_type = "NumberCard"};
          {color = Blue; value = -1; card_type = "Wild"};
          {color = Blue; value = -2; card_type = "PlusFour"};
          {color = Red; value = 12; card_type = "Reverse"};
          {color = Red; value = 10; card_type = "Skip"};
          {color = Red; value = 11; card_type = "PlusTwo"}];
  players = 
    [{is_bot = false; name = "lol";
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      hand = []; playable_cards = []; unod = false; wonnered = false};
     {is_bot = true; name = "lol2";
      hand = [{color = Green; value = 3; card_type = "NumberCard"}];
      playable_cards = [{color = Green; value = 3; card_type = "NumberCard"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false}]; turn = 0; mode = 0;
  stack = []; difficulty = 0}
let after_skip : State.t = {
  before_skip with turn = 1
}
let before_plustwo : State.t = {
  before_skip with discard_pile = 
                     [{color = Red; value = 11; card_type = "PlusTwo"}]
}
let after_plustwo : State.t = {
  before_plustwo with 
  deck = [{color = Blue; value = 4; card_type = "NumberCard"};
          {color = Blue; value = -1; card_type = "Wild"};
          {color = Blue; value = -2; card_type = "PlusFour"};
          {color = Red; value = 12; card_type = "Reverse"};
          {color = Red; value = 10; card_type = "Skip"};
          {color = Red; value = 11; card_type = "PlusTwo"}];
  players = 
    [{is_bot = false; name = "lol";
      hand = [{color = Green; value = 3; card_type = "NumberCard"};
              {color = Red; value = 8; card_type = "NumberCard"}]; 
      playable_cards = [{color = Red; value = 8; card_type = "NumberCard"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false};
     {is_bot = true; name = "lol2";
      hand = [{color = Green; value = 3; card_type = "NumberCard"}];
      playable_cards = [{color = Green; value = 3; card_type = "NumberCard"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false}]; turn = 1;
}
let before_reverse : State.t = {
  before_skip with discard_pile = 
                     [{color = Red; value = 12; card_type = "Reverse"}]
}
let after_reverse : State.t = {
  before_reverse with 
  players = 
    [{is_bot = true; name = "lol2";
      hand = [{color = Green; value = 3; card_type = "NumberCard"}];
      playable_cards = [{color = Green; value = 3; card_type = "NumberCard"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false}; 
     {is_bot = false; name = "lol";
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      hand = []; playable_cards = []; unod = false; wonnered = false};];
  turn = 1;}

let before_wild : State.t = {
  before_skip with discard_pile = 
                     [{color = Wild; value = -1; card_type = "Wild"}]
}

(* states made for [State.gssp] tests *)
let human_player_win_state : State.t = {
  discard_pile = [];
  deck = [];
  players = [{is_bot = false; hand = []; playable_cards = []; unod = false; 
              sock = (stderr,
                      (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
              name = "lol"; wonnered = true}; 
             {is_bot = true; hand = []; playable_cards = []; 
              sock = (stderr,
                      (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
              unod = false; wonnered = false; name = "lol2"}]; turn = 0;
  mode = 0; stack = []; difficulty = 0}
let ai_win_state : State.t = {
  human_player_win_state with 
  players = [{is_bot = false; hand = []; playable_cards = []; unod = false; 
              sock = (stderr,
                      (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
              name = "lol"; wonnered = false}; 
             {is_bot = true; hand = []; playable_cards = []; name = "lol2";
              sock = (stderr,
                      (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
              unod = false; wonnered = true}];}
let no_winner_state : State.t = {
  human_player_win_state with 
  players = [{is_bot = false; hand = []; playable_cards = []; unod = false; 
              sock = (stderr,
                      (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
              name = "lol"; wonnered = false}; 
             {is_bot = true; hand = []; playable_cards = []; name = "lol2";
              sock = (stderr,
                      (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
              unod = false; wonnered = false}];}

(* States made for [State.rename_player] tests *)
let before_rename : State.t = {
  discard_pile = []; deck = [];
  players = [
    {is_bot = false; hand = []; playable_cards = []; unod = false;
     sock = (stderr,
             (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
     name = "target"; wonnered = false;};
    {is_bot = true; hand = []; playable_cards = []; unod = false;
     sock = (stderr,
             (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
     name = "not_target"; wonnered = false;}]; turn = 0; mode = 0; stack = [];
  difficulty = 0;}
let after_rename : State.t = {
  before_rename with players = [
    {is_bot = false; hand = []; playable_cards = []; unod = false;
     sock = (stderr,
             (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
     name = "Sophie"; wonnered = false;};
    {is_bot = true; hand = []; playable_cards = []; unod = false;
     sock = (stderr,
             (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
     name = "not_target"; wonnered = false;}]}

(* States made for [State.get_winner_points] and [State.get_winner_name]
    tests *)
let end_game1 : State.t = {
  before_rename with players = [
    {is_bot = false; hand = []; playable_cards = []; unod = false;
     sock = (stderr,
             (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
     name = "Sophie"; wonnered = true;};
    {is_bot = true; hand = [
         {color = Wild; value = -1; card_type = "Wild"};
         {color = Red; value = 9; card_type = "NumberCard"};
         {color = Blue; value = 10; card_type = "Skip"}
       ]; playable_cards = []; unod = false;
     sock = (stderr,
             (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
     name = "not_target"; wonnered = false;}]}
let end_game2 : State.t = {
  before_rename with players = [
    {is_bot = false; hand = []; playable_cards = []; unod = false;
     sock = (stderr,
             (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
     name = "Sophie"; wonnered = true;};
    {is_bot = true; hand = []; playable_cards = []; unod = false;
     sock = (stderr,
             (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
     name = "not_target"; wonnered = false;}]}

(* States made for [State.play] tests for mode 1 : Stacking *)
let before_play2 : State.t = {
  discard_pile = [{color = Red; value = 3; card_type = "NumberCard"};];
  deck =
    [{color = Blue; value = 3; card_type = "NumberCard"};
     {color = Yellow; value = 3; card_type = "NumberCard"};
     {color = Green; value = 3; card_type = "NumberCard"};
     {color = Red; value = 3; card_type = "NumberCard"}];
  players =
    [{is_bot = false; name = "lol";
      hand = [{color = Green; value = 3; card_type = "NumberCard"};
              {color = Red; value = 8; card_type = "NumberCard"};
              {color = Blue; value = 3; card_type = "NumberCard"};
              {color = Blue; value = -1; card_type = "Wild"};
              {color = Blue; value = -2; card_type = "PlusFour"};
              {color = Red; value = 12; card_type = "Reverse"};
              {color = Red; value = 10; card_type = "Skip"};
              {color = Red; value = 11; card_type = "PlusTwo"}
             ];
      playable_cards = [{color = Green; value = 3; card_type = "NumberCard"};
                        {color = Red; value = 8; card_type = "NumberCard"};
                        {color = Blue; value = 3; card_type = "NumberCard"};
                        {color = Blue; value = -1; card_type = "Wild"};
                        {color = Blue; value = -2; card_type = "PlusFour"};
                        {color = Red; value = 12; card_type = "Reverse"};
                        {color = Red; value = 10; card_type = "Skip"};
                        {color = Red; value = 11; card_type = "PlusTwo"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false};
     {is_bot = true; name = "lol2";
      hand = [{color = Green; value = 3; card_type = "NumberCard"};];
      playable_cards = [{color = Green; value = 3; card_type = "NumberCard"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false}]; turn = 0; mode = 1; 
  stack = []; difficulty = 0}

let after_play2_g3 : State.t = {
  discard_pile = [{color = Green; value = 3; card_type = "NumberCard"};
                  {color = Red; value = 3; card_type = "NumberCard"}];
  deck =
    [{color = Blue; value = 3; card_type = "NumberCard"};
     {color = Yellow; value = 3; card_type = "NumberCard"};
     {color = Green; value = 3; card_type = "NumberCard"};
     {color = Red; value = 3; card_type = "NumberCard"}];
  players =
    [{is_bot = false; name = "lol";
      hand = [{color = Red; value = 8; card_type = "NumberCard"};
              {color = Red; value = 10; card_type = "Skip"};
              {color = Red; value = 11; card_type = "PlusTwo"};
              {color = Red; value = 12; card_type = "Reverse"};
              {color = Blue; value = -2; card_type = "PlusFour"};
              {color = Blue; value = -1; card_type = "Wild"};
              {color = Blue; value = 3; card_type = "NumberCard"}];
      playable_cards = [{color = Blue; value = 3; card_type = "NumberCard"};
                        {color = Blue; value = -1; card_type = "Wild"};
                        {color = Blue; value = -2; card_type = "PlusFour"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false};
     {is_bot = true; name = "lol2";
      hand = [{color = Green; value = 3; card_type = "NumberCard"}];
      playable_cards = [{color = Green; value = 3; card_type = "NumberCard"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false}]; turn = 0; mode = 1; 
  stack = [{color = Green; value = 3; card_type = "NumberCard"};];
  difficulty = 0
}

let after_play2_b3 : State.t = {
  discard_pile = [{color = Blue; value = 3; card_type = "NumberCard"};
                  {color = Green; value = 3; card_type = "NumberCard"};
                  {color = Red; value = 3; card_type = "NumberCard"}];
  deck =
    [{color = Blue; value = 3; card_type = "NumberCard"};
     {color = Yellow; value = 3; card_type = "NumberCard"};
     {color = Green; value = 3; card_type = "NumberCard"};
     {color = Red; value = 3; card_type = "NumberCard"}];
  players =
    [{is_bot = false; name = "lol";
      hand = [{color = Red; value = 8; card_type = "NumberCard"};
              {color = Red; value = 10; card_type = "Skip"};
              {color = Red; value = 11; card_type = "PlusTwo"};
              {color = Red; value = 12; card_type = "Reverse"};
              {color = Blue; value = -2; card_type = "PlusFour"};
              {color = Blue; value = -1; card_type = "Wild"};];
      playable_cards = [{color = Blue; value = -1; card_type = "Wild"};
                        {color = Blue; value = -2; card_type = "PlusFour"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false};
     {is_bot = true; name = "lol2";
      hand = [{color = Green; value = 3; card_type = "NumberCard"}];
      playable_cards = [{color = Green; value = 3; card_type = "NumberCard"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false}]; turn = 0; mode = 1; 
  stack = [{color = Blue; value = 3; card_type = "NumberCard"};
           {color = Green; value = 3; card_type = "NumberCard"}];
  difficulty = 0
}

let b_get_rekd1 : State.t = {
  discard_pile = [{color = Blue; value = 3; card_type = "NumberCard"};
                  {color = Green; value = 3; card_type = "NumberCard"};
                  {color = Red; value = 3; card_type = "NumberCard"}];
  deck =
    [{color = Blue; value = 3; card_type = "NumberCard"};
     {color = Yellow; value = 3; card_type = "NumberCard"};
     {color = Green; value = 3; card_type = "NumberCard"};
     {color = Red; value = 3; card_type = "NumberCard"}];
  players =
    [{is_bot = false; name = "lol";
      hand = [{color = Red; value = 8; card_type = "NumberCard"};
              {color = Red; value = 10; card_type = "Skip"};
              {color = Red; value = 11; card_type = "PlusTwo"};
              {color = Red; value = 12; card_type = "Reverse"};
              {color = Blue; value = -2; card_type = "PlusFour"};
              {color = Blue; value = -1; card_type = "Wild"};];
      playable_cards = [{color = Blue; value = -1; card_type = "Wild"};
                        {color = Blue; value = -2; card_type = "PlusFour"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false};
     {is_bot = true; name = "lol2";
      hand = [{color = Green; value = 3; card_type = "NumberCard"}];
      playable_cards = [{color = Green; value = 3; card_type = "NumberCard"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false}]; turn = 1; mode = 1; 
  stack = [{color = Green; value = 11; card_type = "PlusTwo"}];
  difficulty = 0
}

let get_rekd1 : State.t = {
  discard_pile = [{color = Green; value = 11; card_type = "PlusTwo"};
                  {color = Blue; value = 3; card_type = "NumberCard"};
                  {color = Green; value = 3; card_type = "NumberCard"};
                  {color = Red; value = 3; card_type = "NumberCard"}];
  deck =
    [{color = Green; value = 3; card_type = "NumberCard"};
     {color = Red; value = 3; card_type = "NumberCard"}];
  players =
    [{is_bot = false; name = "lol";
      hand = [{color = Red; value = 8; card_type = "NumberCard"};
              {color = Red; value = 10; card_type = "Skip"};
              {color = Red; value = 11; card_type = "PlusTwo"};
              {color = Red; value = 12; card_type = "Reverse"};
              {color = Blue; value = -2; card_type = "PlusFour"};
              {color = Blue; value = -1; card_type = "Wild"};];
      playable_cards = [{color = Blue; value = -1; card_type = "Wild"};
                        {color = Blue; value = -2; card_type = "PlusFour"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false};
     {is_bot = true; name = "lol2";
      hand = [{color = Green; value = 3; card_type = "NumberCard"};
              {color = Blue; value = 3; card_type = "NumberCard"};
              {color = Yellow; value = 3; card_type = "NumberCard"}];
      playable_cards = [{color = Yellow; value = 3; card_type = "NumberCard"};
                        {color = Blue; value = 3; card_type = "NumberCard"};
                        {color = Green; value = 3; card_type = "NumberCard"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false}]; turn = 0; mode = 1; 
  stack = [];
  difficulty = 0
}

(* States made for [State.pass] tests for mode 2 : Stacking *)
let after_pass_num_stack : State.t = {
  after_play2_b3 with stack = []; turn = 1
}

let before_pass_two_stack : State.t = {
  discard_pile = [{color = Blue; value = 3; card_type = "NumberCard"};
                  {color = Green; value = 3; card_type = "NumberCard"};
                  {color = Red; value = 3; card_type = "NumberCard"}];
  deck =
    [{color = Blue; value = 3; card_type = "NumberCard"};
     {color = Yellow; value = 3; card_type = "NumberCard"};
     {color = Green; value = 3; card_type = "NumberCard"};
     {color = Red; value = 3; card_type = "NumberCard"}];
  players =
    [{is_bot = false; name = "lol";
      hand = [{color = Red; value = 8; card_type = "NumberCard"};
              {color = Red; value = 10; card_type = "Skip"};
              {color = Red; value = 11; card_type = "PlusTwo"};
              {color = Red; value = 12; card_type = "Reverse"};
              {color = Blue; value = -2; card_type = "PlusFour"};
              {color = Blue; value = -1; card_type = "Wild"};];
      playable_cards = [{color = Blue; value = -1; card_type = "Wild"};
                        {color = Blue; value = -2; card_type = "PlusFour"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false};
     {is_bot = true; name = "lol2";
      hand = [{color = Green; value = 3; card_type = "NumberCard"}];
      playable_cards = [{color = Green; value = 3; card_type = "NumberCard"}];
      sock = (stderr,
              (ADDR_INET (inet_addr_of_string "127.0.0.1", 0)));
      unod = false; wonnered = false}]; turn = 0; mode = 1; 
  stack = [{color = Green; value = 11; card_type = "PlusTwo"};
           {color = Blue; value = 11; card_type = "PlusTwo"}];
  difficulty = 0
}
let after_pass_two_stack : State.t = {
  before_pass_two_stack with turn = 1
}

(* States made for [State.play] tests for mode 3: Dos *)
let before_play4 : State.t = {
  before_play2 with mode = 3
}
let after_play4_g3 : State.t = {
  after_play2_g3 with mode = 3
}
let after_play4_b3 : State.t = {
  after_play2_b3 with mode = 3
}
let b_get_rekd3 : State.t = {
  b_get_rekd1 with mode = 3
}
let get_rekd3 : State.t = {
  get_rekd1 with mode = 3
}

(* Decks and cards made for [Deck] tests *)
let exdeck : Deck.deck = [{color = Blue; value = 4; card_type = "NumberCard"};
                          {color = Blue; value = -1; card_type = "Wild"};
                          {color = Blue; value = -2; card_type = "PlusFour"};
                          {color = Red; value = 12; card_type = "Reverse"};
                          {color = Red; value = 10; card_type = "Skip"};
                          {color = Red; value = 11; card_type = "PlusTwo"}]

let cardbl : Deck.card = {color = Blue; value = 7; card_type = "NumberCard"}
let cardre : Deck.card = {color = Red; value = 7; card_type = "NumberCard"}
let cardgr : Deck.card = {color = Green; value = 7; card_type = "NumberCard"}
let cardye : Deck.card = {color = Yellow; value = 7; card_type = "NumberCard"}
let cardwi : Deck.card = {color = Wild; value = -1; card_type = "Wild"}
let cardpl : Deck.card = {color = Wild; value = -2; card_type = "PlusFour"}

let output : (Deck.card list*Deck.deck) = 
  [{color = Blue; value = 4; card_type = "NumberCard"}], 
  [{color = Blue; value = -1; card_type = "Wild"};
   {color = Blue; value = -2; card_type = "PlusFour"};
   {color = Red; value = 12; card_type = "Reverse"};
   {color = Red; value = 10; card_type = "Skip"};
   {color = Red; value = 11; card_type = "PlusTwo"}]

let output2 : (Deck.card list*Deck.deck) = 
  ([{color = Blue; value = -1; card_type = "Wild"};
    {color = Blue; value = 4; card_type = "NumberCard"}],
   [{color = Blue; value = -2; card_type = "PlusFour"};
    {color = Red; value = 12; card_type = "Reverse"};
    {color = Red; value = 10; card_type = "Skip"};
    {color = Red; value = 11; card_type = "PlusTwo"}])

let output3 : (Deck.card list*Deck.deck) = 
  ([{color = Blue; value = -2; card_type = "PlusFour"};
    {color = Blue; value = -1; card_type = "Wild"};
    {color = Blue; value = 4; card_type = "NumberCard"}],
   [{color = Red; value = 12; card_type = "Reverse"};
    {color = Red; value = 10; card_type = "Skip"};
    {color = Red; value = 11; card_type = "PlusTwo"}])

let exdeckbl : Deck.deck = [{color = Blue; value = 7; card_type = "NumberCard"};
                            {color = Blue; value = 4; card_type = "NumberCard"};
                            {color = Blue; value = -1; card_type = "Wild"};
                            {color = Blue; value = -2; card_type = "PlusFour"};
                            {color = Red; value = 12; card_type = "Reverse"};
                            {color = Red; value = 10; card_type = "Skip"};
                            {color = Red; value = 11; card_type = "PlusTwo"}]

let exdeckre : Deck.deck = [{color = Red; value = 7; card_type = "NumberCard"};
                            {color = Blue; value = 4; card_type = "NumberCard"};
                            {color = Blue; value = -1; card_type = "Wild"};
                            {color = Blue; value = -2; card_type = "PlusFour"};
                            {color = Red; value = 12; card_type = "Reverse"};
                            {color = Red; value = 10; card_type = "Skip"};
                            {color = Red; value = 11; card_type = "PlusTwo"}]

let exdeckgr : Deck.deck = [{color = Green; value = 7; card_type = "NumberCard"};
                            {color = Blue; value = 4; card_type = "NumberCard"};
                            {color = Blue; value = -1; card_type = "Wild"};
                            {color = Blue; value = -2; card_type = "PlusFour"};
                            {color = Red; value = 12; card_type = "Reverse"};
                            {color = Red; value = 10; card_type = "Skip"};
                            {color = Red; value = 11; card_type = "PlusTwo"}]

let exdeckye : Deck.deck = 
  [{color = Yellow; value = 7; card_type = "NumberCard"};
   {color = Blue; value = 4; card_type = "NumberCard"};
   {color = Blue; value = -1; card_type = "Wild"};
   {color = Blue; value = -2; card_type = "PlusFour"};
   {color = Red; value = 12; card_type = "Reverse"};
   {color = Red; value = 10; card_type = "Skip"};
   {color = Red; value = 11; card_type = "PlusTwo"}]

let exdeckwi : Deck.deck = [{color = Wild; value = -1; card_type = "Wild"};
                            {color = Blue; value = 4; card_type = "NumberCard"};
                            {color = Blue; value = -1; card_type = "Wild"};
                            {color = Blue; value = -2; card_type = "PlusFour"};
                            {color = Red; value = 12; card_type = "Reverse"};
                            {color = Red; value = 10; card_type = "Skip"};
                            {color = Red; value = 11; card_type = "PlusTwo"}]

let exdeckpl : Deck.deck = [{color = Wild; value = -2; card_type = "PlusFour"};
                            {color = Blue; value = 4; card_type = "NumberCard"};
                            {color = Blue; value = -1; card_type = "Wild"};
                            {color = Blue; value = -2; card_type = "PlusFour"};
                            {color = Red; value = 12; card_type = "Reverse"};
                            {color = Red; value = 10; card_type = "Skip"};
                            {color = Red; value = 11; card_type = "PlusTwo"}]                         

let deck_tests = [
  make_deck_isempty_test "empty deck check" Deck.empty true;
  make_deck_isempty_test "non-empty deck check" exdeck false;
  make_init_test "testing for 108 cards" 108;
  make_emptydeck_test "empty deck" [];
  make_deckdraw_test "drawing from 1 from deck" 1 [] exdeck output;
  make_deckdraw_test "drawing from 2 from deck" 2 [] exdeck output2;
  make_deckdraw_test "drawing from 3 from deck" 3 [] exdeck output3;
  make_push_test "pushing a blue card onto a deck" cardbl exdeck exdeckbl;
  make_push_test "pushing a red card onto a deck" cardre exdeck exdeckre;
  make_push_test "pushing a green card onto a deck" cardgr exdeck exdeckgr;
  make_push_test "pushing a yellow card onto a deck" cardye exdeck exdeckye;
  make_push_test "pushing a wild card onto a deck" cardwi exdeck exdeckwi;
  make_push_test "pushing a plusfour card onto a deck" cardpl exdeck exdeckpl;
  make_peek_test "testing peek on a top blue deck" exdeckbl cardbl;
  make_peek_test "testing peek on a top red deck" exdeckre cardre;
  make_peek_test "testing peek on a top green deck" exdeckgr cardgr;
  make_peek_test "testing peek on a top yellow deck" exdeckye cardye;
  make_peek_test "testing peek on a top wild deck" exdeckwi cardwi;
  make_peek_test "testing peek on a top wildplus deck" exdeckpl cardpl;
]

let state_tests = [
  make_draw_test "draw 2 cards from normal deck" before1_draw 2 after1_draw_2;
  make_draw_shuffled_test "not enough cards in deck, shuffles the discard pile
  to form new deck" before2_draw 1 2;
  make_play_test "after playing green 3" before_play 
    {color = Green; value = 3; card_type = "NumberCard"} after_play_green3;
  make_play_test "after playing red 8" before_play
    {color = Red; value = 8; card_type = "NumberCard"} after_play_red8;
  make_play_test "after playing Wild Blue" before_play
    {color = Blue; value = -1; card_type = "Wild"} after_play_bluewild;
  make_play_test "after playing PlusFour Blue" before_play
    {color = Blue; value = -2; card_type = "PlusFour"} after_play_blueplusfour;
  make_play_test "after playing red reverse" before_play
    {color = Red; value = 12; card_type = "Reverse"} after_play_redreverse;
  make_play_test "after playing red skip" before_play
    {color = Red; value = 10; card_type = "Skip"} after_play_redskip;
  make_play_test "after playing red PlusTwo" before_play
    {color = Red; value = 11; card_type = "PlusTwo"} after_play_redplustwo;
  make_play_exception_test 
    "[InvalidMove] because player does not have this card" before_play 
    {color = Yellow; value = 0; card_type = "NumberCard"};
  make_play_exception_test 
    "[InvalidMove] because player has this card but is not playable given the
    current top card" before_play 
    {color = Blue; value = 4; card_type = "NumberCard"};
  make_change_color_test "changes the top wild card to blue" unchanged_wild_st
    Blue changed_wild_st;
  make_change_wildcard_color_test "changes the wild card color from wild to
    green" unchanged_player_with_wild_st 
    {color = Wild; value = -1; card_type = "Wild"} Green 
    changed_player_with_wild_st;
  make_change_wildcard_color_test "if the player does not have a wild card, the
    state does not change, (order does not matter)" player_wo_wild_st
    {color = Wild; value = -1; card_type = "Wild"} Green player_wo_wild_st2;
  make_init_top_effect_test "case when top card flipped is skip" before_skip
    after_skip;
  make_init_top_effect_test "case when top card flipped is PlusTwo" 
    before_plustwo after_plustwo;
  make_init_top_effect_test "case when top card flipped is reverse" 
    before_reverse after_reverse;
  make_itet_exception_test "case when top card flipped is Wild, raises
    [PendingColor]" before_wild;
  make_gsow_test "human player wins at this state" human_player_win_state true;
  make_gsow_exception_test "[GameOver] because AI wins at this state"
    ai_win_state;
  make_gsow_test "no winner yet, game continues" no_winner_state false;
  make_pass_test "turn passed on to next player" bt_st pass_st;
  make_uno_test "current player called uno" bt_st uno_st;
  make_rename_test "renaming current player to 'Sophie'" before_rename "Sophie"
    after_rename;
  make_winner_pts_test "50 + 20 + 9 = 79" end_game1 79;
  make_winner_pts_test "0" end_game2 0;
  make_winner_name_test "Sophie wins" end_game1 "Sophie";
  make_play_test "mode Stacking stack green 3" before_play2
    {color = Green; value = 3; card_type = "NumberCard"} after_play2_g3;
  make_play_test "mode Stacking stack blue 3" after_play2_g3
    {color = Blue; value = 3; card_type = "NumberCard"} after_play2_b3;
  make_play_exception_test "not stackable" after_play2_g3 
    {color = Green; value = 9; card_type = "NumberCard"};
  make_play_ex_stack_test "cannot play, gets wrecked" b_get_rekd1
    {color = Green; value = 3; card_type = "NumberCard"};
  make_draw_test "cannot draw, gets wrecked" b_get_rekd1 1 get_rekd1;
  make_pass_test "passing after stacking numbers" after_play2_b3
    after_pass_num_stack;
  make_pass_test "passing after stacking two's" before_pass_two_stack
    after_pass_two_stack;
  make_play_test "mode Dos stack green 3" before_play4
    {color = Green; value = 3; card_type = "NumberCard"} after_play4_g3;
  make_play_test "mode Dos stack blue 3" after_play4_g3
    {color = Blue; value = 3; card_type = "NumberCard"} after_play4_b3;
  make_play_exception_test "not stackable : Dos" after_play4_g3 
    {color = Green; value = 9; card_type = "NumberCard"};
  make_play_ex_stack_test "cannot play, gets wrecked : Dos" b_get_rekd3
    {color = Green; value = 3; card_type = "NumberCard"};
  make_draw_test "cannot draw, gets wrecked : Dos" b_get_rekd3 1 get_rekd3;
]

let suite =
  "test suite for Uno"  >::: List.flatten [
    parse_tests;
    state_tests;
    deck_tests;
  ]

let _ = run_test_tt_main suite
