open Game
open Money

let acc = Money.create_account ()

let rec menu () =
  print_endline "\nChoose a game to play:";
  print_endline "1. Blackjack";
  print_endline "2. Slotmachine";
  print_endline "3. Baccarat";
  print_endline "4. Roulette";
  print_endline "5. Craps";
  print_endline "6. Match Three";
  print_endline "7. Quit";
  match read_line () with
  | "1" ->
      Random.self_init ();
      print_endline "Welcome to Blackjack!";
      let deck = Blackjack.create_deck () in
      let b = Money.bet acc in
      Blackjack.game_loop
        (deck |> List.tl |> List.tl)
        []
        [ List.hd deck; List.nth deck 1 ]
        acc b;
      print_endline "Thanks for playing!";
      menu ()
  | "2" ->
      print_endline "How many times do you want to spin? ";
      let n = read_int () in
      let _ = Money.fixed_bet acc 5.0 n in
      let wins = ref 0 in
      (* new variable to keep track of wins *)
      for i = 1 to n do
        print_endline ("\nSpin #" ^ string_of_int i ^ ":");
        let result = Slotmachine.spin_slot_machine () in
        Slotmachine.print_spin_result result;
        if Slotmachine.is_win result then (
          print_endline "You win!";
          wins := !wins + 1 (* increment wins counter *))
      done;
      print_endline ("You won " ^ string_of_int !wins ^ " times.");
      Money.process_transaction acc (Deposit (float_of_int !wins *. 15.0));
      print_endline
        ("Your balance is " ^ string_of_float (Money.get_balance acc))
  | "3" ->
      print_endline "\nWelcome to Baccarat!";
      Baccarat.play_baccarat acc;
      menu ()
  | "4" ->
      print_endline "\nWelcome to European Roulette!";
      Roulette.play_roulette acc;
      menu ()
  | "5" ->
      print_endline "\nWelcome to Craps!";
      Craps.play_craps acc;
      menu ()
  | "6" ->
      print_endline "\nWelcome to Match Three!";
      Matchthree.play_game ();
      menu ()
  | "7" -> print_endline "Thanks for playing!"
  | _ ->
      print_endline "Invalid input, please enter 1 or 2.";
      menu ()

let () = menu ()
