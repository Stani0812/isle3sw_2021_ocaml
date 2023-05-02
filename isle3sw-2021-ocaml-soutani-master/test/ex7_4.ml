open Tester
module T = Testtools
module Ex = Exercise.Main


let () = ignore (run_test_case {
      name  = "Ex.7.4";
      func  = Ex.fact_imp;
      prep  = T.id;
      ishow = string_of_int;
      oshow = string_of_int;
      cmp   = (=);
      dataset = [
          { input = 0; expected = 1 };
          { input = 1; expected = 1 };
          { input = 2; expected = 2 };
          { input = 3; expected = 6 };
          { input = 4; expected = 24 };
          { input = 5; expected = 120 };
          { input = 6; expected = 720 };
          { input = 10; expected = 3628800 };
      ]
})
