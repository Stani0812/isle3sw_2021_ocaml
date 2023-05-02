open Tester
module T = Testtools
module Ex = Exercise.Main


let () = ignore (run_test_case {
      name  = "Ex.5.3(4)";
      func  = Ex.zip;
      prep  = T.uncurry2;
      ishow = T.str_pair (T.str_list string_of_int) (T.str_list string_of_int);
      oshow = T.str_list (T.str_pair string_of_int string_of_int);
      cmp   = (=);
      dataset = [
          { input = ([], []); expected = [] };
          { input = ([], [1; 2; 3]); expected = [] };
          { input = ([1; 2; 3], []); expected = [] };
          { input = ([1; 2; 8], [3; 4; 9; 2]); expected = [(1, 3); (2, 4); (8, 9)] };
          { input = ([1; 2; 8; 6; 1], [4; 9; 2]); expected = [(1, 4); (2, 9); (8, 2)] };
      ]
})
