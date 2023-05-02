open Tester
module T = Testtools
module Ex = Exercise.Main


let () = ignore (run_test_case {
      name  = "Ex.5.3(1)";
      func  = Ex.downto0;
      prep  = T.id;
      ishow = string_of_int;
      oshow = T.str_list string_of_int;
      cmp   = (=);
      dataset = [
          { input = 1; expected = [1; 0] };
          { input = 2; expected = [2; 1; 0] };
          { input = 3; expected = [3; 2; 1; 0] };
          { input = 10; expected = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0] };
          { input = 20; expected = [20; 19; 18; 17; 16; 15; 14; 13; 12; 11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0] };
      ]
})
