(** Test Plan.

    The OUnit test suite is developed to test the helper functions behind the 
    games. It is worth pointing out that the interface of the games and any function
    that involve input-output operations are tested manually, such as 
    game_loop functions. All test cases are developed using either black box or 
    glass box testing.
    
    The strategies, correctness, and not-tested features of all modules are illustrated
    as follows:
    
    Baccarat.ml
    - Strategy: Black box testing was utilized to test helper functions string_
    of_card, string_of_card_and_suit, string_of_hand, make_deck, shuffle_deck, 
    deal_cards, score_card, and score_hand. Some cards are tested to make sure 
    their corresponding scores, values, and suits are returned correctly. Then, 
    glass box testing is employed to test compare_hands.
    
    - Correctness: The core functionality behind baccarat is that scores from the
    banker and the player must be calculated correctly. Hence, the following cases
    are considered: no 3rd card needs to be drawn, player draws the 3rd card, and 
    both player and banker draws 3rd card. In each case, the test validates the 
    correctness of the program.

    - Not tested: place_bet and play_baccaarat are manually tested by playing the game.
    
    Blackjack.ml
    - Strategy: Black box tests are utilized to test string_of_rank, string_of_suit,
    create_deck, has_duplicates, and shuffle. Glass box tests are utilized to test
    score. In particular, some cards are drawn from a deck and checked against their
    expected value and suit. The shuffle function is checked to ensure the order 
    of cards changed.

    - Correctness: The result of a blackjack game is determined by the score function,
    which assigns scores to a hand. Hense, the glass box tests considers a variety 
    of hands a player could have, such as an empty hand, a hand with more than 21 points,
    or a hand less than 21 points. The score function justifies the correctness of
    the results of the game.

    - Not tested: player_turn, dealer_turn, and game_loop are tested manually by 
    playing the game.
    
    Craps.ml
    - Strategy: Black box tests are utilized to test roll_dice. The function checks
    to make sure the result of each rolling gives a valid integer between 1 and 6.

    - Correctness: The Craps is a simple game that compares the results of rolling 
    dice between players. Hence, the result of roll_dice shows that the game is fair.

    - Not tested: play_craps is checked by playing the game.
    
    Matchthree.ml
    - Strategy: Black box tests are utilized to test init_board, check_empty, swap_items,
    vertical_shift. Glass box tests are utilized to test check_matches. First, the
    tests check that items have valid colors and coordinates after initialization.
    Then, it is checked tha the board randomizes in each round by making sure they
    are not equal. swap_items is tested to make sure items are actually swaped. 
    check_matches function is tested using glass_box testing by considering the cases
    in which balls may be matched.

    - Correctness: The match three game works by first randomly creating a board. 
    This is tested in the comparison of two new boards. Then, after each move, the 
    board should be changed by vertically shifting some items down. These cases are
    checked in the check_matches_tests by considering various circumstances in which
    balls may be matched and removed from board.

    - Not tested: game_loop, play_game, and io_location are checked by playing the game.
    
    Money.ml
    - Strategy: Glass box testing is utilized to check the core functions in the 
    money module, including create_account and process_transaction.

    - Correctness: Initially, every player begins with 1000 dollars in bank. This 
    is checked in the create_account tests. Then, a player can withdraw of deposit 
    certain amounts of money. If the player intends to withdraw more money than 
    they have, the balance will not change. This ensures the correctness of the 
    monetary operations in all games.

    - Not tested: bet and fixed_bet are checked by playing the games that involve 
    these features.
    
    Roulette.ml
    - Strategy: Black box testing is utilized to test spin_wheel and Glass box 
    testing is used to test calculate_payout. The implementer considers various cases
    in which payouts may be calculated and compare the expected payouts with the actual
    ones.

    - Correctness: The correctness of this module depends on a valid spinning of 
    wheel and an accurate calculation of the payout. The spin_wheel ensures that the 
    result of each spinning is valid. Then, the payouts for even, odd, black, red,
    numbers, of columns are each tested to make sure the correct amounts of money 
    are returned.

    - Not tested: play_roulette is tested by playing the game.
    
    Slotmachine.ml 
    - Strategy: Glass box testing is utilized to spin_slot_machine, main, and is_win.

    - Correctness: The correctness of this module depends on a valid spinning of 
    the slot machine and the determination of whether the play has won the game. 
    The spining is tested by checking whether the returned machin has the correct 
    length and format. Then, is_win checks is a winnning and losing pattern match
    with their expected results.

    - Not tested: print_spin_result and main are tested by playing the game.*)

(**********************************************************************
 **********************************************************************)
open OUnit2
open Game
open Baccarat
open Matchthree
open Roulette
open Craps

let rec has_duplicates lst =
  match lst with [] -> false | x :: xs -> List.mem x xs || has_duplicates xs

(* An unshuffled deck with 52 cards *)
let deck = make_deck ()

(* A shuffled deck with 52 cards *)
let deck_s = deck |> shuffle_deck

let baccarat_tests =
  let open Baccarat in
  [
    ( "test string_of_card" >:: fun _ ->
      assert_equal "A" (string_of_card (Ace, Spades));
      assert_equal "K" (string_of_card (King, Diamonds));
      assert_equal "7" (string_of_card (Seven, Clubs)) );
    ( "test string_of_card_and_suit" >:: fun _ ->
      assert_equal "A of Spades" (string_of_card_and_suit (Ace, Spades));
      assert_equal "K of Diamonds" (string_of_card_and_suit (King, Diamonds));
      assert_equal "7 of Clubs" (string_of_card_and_suit (Seven, Clubs)) );
    ( "test string_of_hand" >:: fun _ ->
      assert_equal "" (string_of_hand []);
      assert_equal "A of Spades" (string_of_hand [ (Ace, Spades) ]);
      assert_equal "A of Spades, 7 of Clubs"
        (string_of_hand [ (Ace, Spades); (Seven, Clubs) ]) );
    ("test make_deck" >:: fun _ -> assert_equal 52 (List.length (make_deck ())));
    ( "test make_deck has no duplicates" >:: fun _ ->
      assert_equal false (has_duplicates (make_deck ())) );
    ( "test shuffle_deck" >:: fun _ ->
      let deck2 = shuffle_deck deck in
      assert_equal 52 (List.length deck2);
      assert_bool "shuffled deck not equal to original deck" (deck <> deck2) );
    ( "test deal_cards" >:: fun _ ->
      let hand = [] in
      let deck', hand' = deal_cards 2 deck hand in
      assert_equal 50 (List.length deck');
      assert_equal 2 (List.length hand') );
    ( "test score_card" >:: fun _ ->
      assert_equal 1 (score_card Ace);
      assert_equal 0 (score_card Jack);
      assert_equal 0 (score_card Queen);
      assert_equal 0 (score_card King);
      assert_equal 9 (score_card Nine) );
    ( "test score_hand" >:: fun _ ->
      assert_equal 3 (score_hand [ (Ace, Spades); (Two, Clubs) ]);
      assert_equal 0 (score_hand [ (Ace, Spades); (Nine, Diamonds) ]);
      assert_equal 4 (score_hand [ (Six, Hearts); (Eight, Spades) ]) );
    ( "test compare_hands that no 3rd card needs to be drawn" >:: fun _ ->
      let player_hand = [ (Three, Clubs); (Three, Diamonds) ] in
      let banker_hand = [ (Four, Clubs); (Three, Spades) ] in
      let deck' =
        List.filter
          (fun x ->
            x <> (Three, Clubs)
            || x <> (Three, Diamonds)
            || x <> (Four, Clubs)
            || x <> (Three, Spades))
          deck_s
      in
      assert_equal
        ("Banker wins!", player_hand, banker_hand)
        (compare_hands player_hand banker_hand deck') );
    ( "test compare_hands that player needs draw 3rd card" >:: fun _ ->
      let player_hand = [ (Three, Clubs); (Two, Diamonds) ] in
      (* 5 + 3 = 8*)
      let banker_hand = [ (Four, Clubs); (Three, Spades) ] in
      (* 7 *)
      let deck' =
        List.filter
          (fun x ->
            x <> (Three, Clubs)
            || x <> (Three, Diamonds)
            || x <> (Two, Diamonds)
            || x <> (Four, Clubs)
            || x <> (Three, Spades))
          deck_s
      in
      let deck'' = (Three, Diamonds) :: deck' in
      let player_hand' = List.rev ((Three, Diamonds) :: player_hand) in
      assert_equal
        ("Player wins!", player_hand', banker_hand)
        (compare_hands player_hand banker_hand deck'') );
    ( "test compare_hands that player needs draw 3rd card and banker may also do"
    >:: fun _ ->
      let player_hand = [ (Three, Clubs); (Two, Diamonds) ] in
      (* 5 + 3 = 8*)
      let banker_hand = [ (Five, Clubs); (Jack, Spades) ] in
      (* 5 *)
      let deck' =
        List.filter
          (fun x ->
            x <> (Three, Clubs)
            || x <> (Three, Diamonds)
            || x <> (Two, Diamonds)
            || x <> (Jack, Spades))
          deck_s
      in
      let deck'' = (Three, Diamonds) :: deck' in
      let player_hand' = List.rev ((Three, Diamonds) :: player_hand) in
      assert_equal
        ("Player wins!", player_hand', banker_hand)
        (compare_hands player_hand banker_hand deck'') );
  ]

let roulette_tests =
  [
    ( "test spin_wheel" >:: fun _ ->
      assert_bool "output is between 0 and 36"
        (spin_wheel () >= 0 && spin_wheel () <= 36) );
    ( "calculate_payout even 20 2 should return 40" >:: fun _ ->
      assert_equal 40 (calculate_payout "even" 20 2) );
    ( "calculate_payout odd 20 2 should return 0" >:: fun _ ->
      assert_equal 0 (calculate_payout "odd" 20 2) );
    ( "calculate_payout red 20 2 should return 0" >:: fun _ ->
      assert_equal 0 (calculate_payout "red" 20 2) );
    ( "calculate_payout black 20 2 should return 40" >:: fun _ ->
      assert_equal 40 (calculate_payout "even" 20 2) );
    ( "calculate_payout 1to18 20 2 should return 40" >:: fun _ ->
      assert_equal 40 (calculate_payout "1to18" 20 2) );
    ( "calculate_payout 19to36 20 2 should return 0" >:: fun _ ->
      assert_equal 0 (calculate_payout "19to36" 20 2) );
    ( "calculate_payout 1st12 20 15 should return 0" >:: fun _ ->
      assert_equal 0 (calculate_payout "1st12" 20 15) );
    ( "calculate_payout 2nd12 20 15 should return 60" >:: fun _ ->
      assert_equal 60 (calculate_payout "2nd12" 20 15) );
    ( "calculate_payout 3rd12 20 15 should return 0" >:: fun _ ->
      assert_equal 0 (calculate_payout "3rd12" 20 15) );
    ( "calculate_payout 1stcol 20 15 should return 0" >:: fun _ ->
      assert_equal 0 (calculate_payout "1stcol" 20 15) );
    ( "calculate_payout 2ndcol 20 14 should return 60" >:: fun _ ->
      assert_equal 60 (calculate_payout "2ndcol" 20 14) );
    ( "calculate_payout 3rdcol 20 14 should return 60" >:: fun _ ->
      assert_equal 0 (calculate_payout "3rdcol" 20 14) );
  ]

let craps_tests =
  [
    ( "test roll_dice" >:: fun _ ->
      let d1 = roll_dice () |> fst in
      let d2 = roll_dice () |> snd in
      assert_bool "each dice is between 1 and 6"
        (d1 >= 1 && d1 <= 6 && d2 >= 1 && d2 <= 6) );
  ]

let string_of_rank_test (name : string) (rk : Blackjack.rank)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Blackjack.string_of_rank rk)

let blackjack_tests =
  let open Blackjack in
  [
    string_of_rank_test "trivial case Two to 2" Two "2";
    string_of_rank_test "trivial case Ace to A" Ace "A";
    string_of_rank_test "trivial case Jack to J" Jack "J";
    ( "string_of_suit Clubs is Clubs" >:: fun _ ->
      assert_equal "Clubs" (string_of_suit Clubs) );
    ( "check create_deck create 52 cards" >:: fun _ ->
      assert_equal 52 (List.length (create_deck ())) );
    ( "check create_deck has no duplicate cards" >:: fun _ ->
      assert_equal false (has_duplicates (create_deck ())) );
    ( "check shuffle" >:: fun _ ->
      assert_bool "shuffle changes the order of a deck"
        (create_deck () <> (create_deck () |> shuffle)) );
    ("score empty hand should be 0" >:: fun _ -> assert_equal 0 (score []));
    ( "score hand with one Ace should be 11" >:: fun _ ->
      assert_equal 11 (score [ (Ace, Hearts) ]) );
    ( "score hand with two Aces should be 12" >:: fun _ ->
      assert_equal 12 (score [ (Ace, Hearts); (Ace, Clubs) ]) );
    ( "score hand with two Aces and a King should be 12" >:: fun _ ->
      assert_equal 12 (score [ (Ace, Hearts); (Ace, Clubs); (King, Diamonds) ])
    );
  ]

let money_tests =
  let open Money in
  [
    ( "check balance of initial account" >:: fun _ ->
      assert_equal 1000.0 (create_account () |> get_balance) );
    ( "process deposit" >:: fun _ ->
      let a = create_account () in
      let () = Deposit 50.0 |> process_transaction a in
      assert_equal 1050.0 (get_balance a) );
    ( "process invalid withdral" >:: fun _ ->
      let a = create_account () in
      let () = Withdrawal 1080.0 |> process_transaction a in
      assert_equal 1000.0 (get_balance a) );
    ( "process valid withdral" >:: fun _ ->
      let a = create_account () in
      let () = Withdrawal 80.0 |> process_transaction a in
      assert_equal 920.0 (get_balance a) );
  ]

let test_spin_slot_machine _ =
  let result = Slotmachine.spin_slot_machine () in
  assert_equal 3 (List.length result);
  List.iter
    (fun s ->
      assert_bool "invalid symbol" (List.mem s [ "🍒"; "🍇"; "🍊"; "🍉"; "🔔" ]))
    result

let test_is_win _ =
  let win_row = [ "🍒"; "🍒"; "🍒" ] in
  let lose_row = [ "🍒"; "🍇"; "🍊" ] in
  assert_bool "winning row not detected" (Slotmachine.is_win win_row);
  assert_bool "losing row detected as win" (not (Slotmachine.is_win lose_row))

let slotmachine_tests =
  [
    "test_spin_slot_machine" >:: test_spin_slot_machine;
    "test_is_win" >:: test_is_win;
  ]

let item_tests =
  let test1 =
    "item color test" >:: fun _ ->
    let item = { color = "⚽️"; x = 1; y = 1 } in
    assert_equal "⚽️" item.color
  in
  let test2 =
    "item coordinate test" >:: fun _ ->
    let item = { color = "⚽️"; x = 1; y = 2 } in
    assert_equal 1 item.x;
    assert_equal 2 item.y
  in
  [ test1; test2 ]

let board_tests =
  let test1 =
    "board size test" >:: fun _ ->
    let board = init_board 8 8 in
    assert_equal 8 (Array.length board);
    Array.iter (fun row -> assert_equal 8 (Array.length row)) board
  in
  let test2 =
    "board randomization test" >:: fun _ ->
    let board1 = init_board 8 8 in
    let board2 = init_board 8 8 in
    assert_bool "board randomization" (board1 <> board2)
  in
  let test3 =
    "board swap test" >:: fun _ ->
    let board = init_board 8 8 in
    let item1 = board.(0).(0) in
    let item2 = board.(0).(1) in
    swap_items board 0 0 0 1;
    assert_equal item1 board.(0).(1);
    assert_equal item2 board.(0).(0)
  in
  [ test1; test2; test3 ]

let make_board () =
  let board = init_board 8 8 in
  board.(0).(0) <- { color = "⚽️"; x = 0; y = 0 };
  board.(0).(1) <- { color = "⚽️"; x = 0; y = 1 };
  board.(0).(2) <- { color = "🏀"; x = 0; y = 2 };
  board.(1).(0) <- { color = "🏀"; x = 1; y = 0 };
  board.(1).(1) <- { color = "🏀"; x = 1; y = 1 };
  board.(1).(2) <- { color = "🏀"; x = 1; y = 2 };
  board

let check_matches_tests =
  let board = make_board () in
  "check_matches"
  >::: [
         ( "returns None if no matches" >:: fun _ ->
           assert_equal None (check_matches board board.(0).(0) 0);
           assert_equal None (check_matches board board.(0).(2) 0) );
         ( "returns Some triple if matches" >:: fun _ ->
           print_board board;
           assert_equal
             (Some
                ( { color = "🏀"; x = 1; y = 0 },
                  { color = "🏀"; x = 1; y = 1 },
                  { color = "🏀"; x = 1; y = 2 } ))
             (check_matches board board.(1).(0) 0) );
       ]

let matchthree_tests =
  List.flatten [ item_tests; board_tests ] @ [ check_matches_tests ]

let tests =
  "test suite for A1"
  >::: List.flatten
         [
           blackjack_tests;
           money_tests;
           baccarat_tests;
           slotmachine_tests;
           matchthree_tests;
           roulette_tests;
           craps_tests;
         ]

let _ = run_test_tt_main tests
