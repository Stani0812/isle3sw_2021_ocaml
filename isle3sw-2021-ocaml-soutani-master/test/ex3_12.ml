open Tester
module T = Testtools
module Ex = Exercise.Main

let _ = run_test_case {
      name  = "Ex.3.12";
      func  = Ex.arctan_one;
      prep  = T.id;
      ishow = string_of_int;
      oshow = T.string_of_float10;
      cmp   = T.eq_float;
      dataset = [
          { input = -1; expected = 0.000000000000 };
          { input = 0; expected = 0.666666666667 };
          { input = 1; expected = 0.72380952381 };
          { input = 2; expected = 0.744011544012 };
          { input = 3; expected = 0.754267954268 };
          { input = 4; expected = 0.760459904732 };
          { input = 5; expected = 0.764600691482 };
          { input = 10; expected = 0.774040381616 };
          { input = 100; expected = 0.784160547218 };
          { input = 10000; expected = 0.785385664647 };
      ]
}
