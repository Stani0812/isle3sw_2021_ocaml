open Tester
module T = Testtools
module Ex = Exercise.Main

let _ = run_test_case {
      name  = "Ex.3.11(3)";
      func  = Ex.fib_iter;
      prep  = T.id;
      ishow = string_of_int;
      oshow = string_of_int;
      cmp   = (=);
      dataset = [
          { input = 1; expected = 1 };
          { input = 2; expected = 1 };
          { input = 3; expected = 2 };
          { input = 4; expected = 3 };
          { input = 5; expected = 5 };
          { input = 6; expected = 8 };
          { input = 7; expected = 13 };
          { input = 8; expected = 21 };
          { input = 9; expected = 34 };
          { input = 10; expected = 55 };
          { input = 50; expected = 12586269025 };
          { input = 80; expected = 23416728348467685 };
      ]
}
