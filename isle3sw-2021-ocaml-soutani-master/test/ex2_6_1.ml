open Tester
module T = Testtools
module Ex = Exercise.Main

let () = ignore (run_test_case {
      name  = "Ex.2.6(1)";
      func   = Ex.yen_of_usdollar;
      prep  = T.id;
      ishow = string_of_float;
      oshow = string_of_int;
      cmp = (=);
      dataset = [
          { input = 0.; expected = 0 };
          { input = 10.; expected = 1111 };
          { input = 40.; expected = 4445 };
          { input = 117.; expected = 13001 };
          { input = 259.; expected = 28780 };
          { input = 712.; expected = 79117 };
          { input = 5000.; expected = 555600 };
          { input = 6.24; expected = 693 };
          { input = 6.25; expected = 695 };
          { input = 6.26; expected = 696 };
          { input = 92.6; expected = 10290 };
          { input = 34.16; expected = 3796 };
          { input = 59.78; expected = 6643 };
          { input = 86.68; expected = 9632 };
          { input = 78.66; expected = 8741 };
          { input = 62.53; expected = 6948 };
          { input = 9.09; expected = 1010 };
          { input = 31.1; expected = 3456 };
          { input = 55.97; expected = 6219 };
          { input = 15.38; expected = 1709 };
          { input = 0.97; expected = 108 };
          { input = 75.66; expected = 8407 };
          { input = 24.83; expected = 2759 };
          { input = 65.27; expected = 7253 };
          { input = 98.71; expected = 10969 };
          { input = 16.74; expected = 1860 };
          { input = 55.74; expected = 6194 };
          { input = 75.85; expected = 8428 };
          { input = 68.53; expected = 7615 };
          { input = 46.75; expected = 5195 };
          { input = 32.83; expected = 3648 };
          { input = 72.52; expected = 8058 };
          { input = 76.08; expected = 8454 };
          { input = 94.45; expected = 10495 };
          { input = 45.33; expected = 5037 };
          { input = 53.96; expected = 5996 };
          { input = 86.81; expected = 9646 };
          { input = 54.49; expected = 6055 };
          { input = 63.02; expected = 7003 };
          { input = 7.81; expected = 868 };
          { input = 12.47; expected = 1386 };
          { input = 47.72; expected = 5303 };
          { input = 27.07; expected = 3008 };
          { input = 88.94; expected = 9883 };
          { input = 30.08; expected = 3342 };
          { input = 28.21; expected = 3135 };
          { input = 55.65; expected = 6184 };
          { input = 10.47; expected = 1163 };
          { input = 60.39; expected = 6711 };
          { input = 86.04; expected = 9561 };
      ]
})
