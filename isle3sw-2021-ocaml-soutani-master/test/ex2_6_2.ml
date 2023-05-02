open Tester
module T = Testtools
module Ex = Exercise.Main

let () = ignore (run_test_case {
    name  = "Ex.2.6(2)";
    func   = Ex.usdollar_of_yen;
    prep  = T.id;
    ishow = string_of_int;
    oshow = string_of_float;
    cmp  = T.eq_float;
      dataset = [
          { input = 0; expected = 0. };
          { input = 1; expected = 0.01 };
          { input = 2; expected = 0.02 };
          { input = 3; expected = 0.03 };
          { input = 5; expected = 0.04 };
          { input = 8; expected = 0.07 };
          { input = 13; expected = 0.12 };
          { input = 21; expected = 0.19 };
          { input = 34; expected = 0.31 };
          { input = 55; expected = 0.49 };
          { input = 89; expected = 0.8 };
          { input = 2069; expected = 18.62 };
          { input = 3095; expected = 27.85 };
          { input = 6357; expected = 57.21 };
          { input = 7092; expected = 63.82 };
          { input = 824; expected = 7.42 };
          { input = 146; expected = 1.31 };
          { input = 6278; expected = 56.5 };
          { input = 6133; expected = 55.19 };
          { input = 8688; expected = 78.19 };
          { input = 4989; expected = 44.9 };
          { input = 8295; expected = 74.65 };
          { input = 3158; expected = 28.42 };
          { input = 8393; expected = 75.53 };
          { input = 601; expected = 5.41 };
          { input = 2456; expected = 22.1 };
          { input = 7513; expected = 67.61 };
          { input = 7329; expected = 65.96 };
          { input = 1013; expected = 9.12 };
          { input = 8000; expected = 71.99 };
          { input = 2017; expected = 18.15 };
          { input = 3186; expected = 28.67 };
          { input = 5571; expected = 50.13 };
          { input = 9024; expected = 81.21 };
          { input = 9348; expected = 84.13 };
          { input = 1836; expected = 16.52 };
          { input = 11; expected = 0.1 };
          { input = 5671; expected = 51.03 };
          { input = 4232; expected = 38.08 };
          { input = 6465; expected = 58.18 };
          { input = 7649; expected = 68.84 };
          { input = 3931; expected = 35.38 };
          { input = 3450; expected = 31.05 };
          { input = 5569; expected = 50.12 };
          { input = 958; expected = 8.62 };
          { input = 4212; expected = 37.9 };
          { input = 8283; expected = 74.54 };
          { input = 2101; expected = 18.91 };
          { input = 4696; expected = 42.26 };
          { input = 809; expected = 7.28 };
          { input = 1461; expected = 13.15 };
      ]
})
