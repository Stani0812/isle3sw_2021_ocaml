open Tester
module T = Testtools
module Ex = Exercise.Main

let () = ignore (run_test_case {
      name  = "Ex.4.5";
      func  = Ex.fib_repeat;
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
          { input = 10; expected = 55 };
          { input = 20; expected = 6765 };
          { input = 30; expected = 832040 };
          { input = 40; expected = 102334155 };
          { input = 50; expected = 12586269025 };
          { input = 60; expected = 1548008755920 };
      ]
})
