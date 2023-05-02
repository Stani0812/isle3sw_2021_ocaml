open Tester
module T = Testtools
module Ex = Exercise.Main


let () = ignore (run_test_case {
      name  = "Ex.5.3(3)";
      func  = Ex.concat;
      prep  = T.id;
      ishow = T.str_list (T.str_list string_of_int);
      oshow = T.str_list string_of_int;
      cmp   = (=);
      dataset = [
          { input = []; expected = [] };
          { input = [[]]; expected = [] };
          { input = [[1]]; expected = [1] };
          { input = [[]; [1]; []; [2]; []]; expected = [1; 2] };
          { input = [[0; 3; 4]; [2]; [5; 0]; []]; expected = [0; 3; 4; 2; 5; 0] };
          { input = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9; 10]]; expected = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] };
      ]
})
