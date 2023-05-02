open Tester
module T = Testtools
module Ex = Exercise.Main


let () = ignore (run_test_case {
      name  = "Ex.5.3(5)";
      func  = Ex.filter;
      prep  = T.uncurry2;
      ishow = T.str_pair T.str_fun (T.str_list string_of_int);
      oshow = T.str_list string_of_int;
      cmp   = (=);
      dataset = [
          { input = ((fun x -> x mod 2 = 0), [0; 1; 2; 3; 4; 5; 6]); expected = [0; 2; 4; 6] };
          { input = ((fun x -> x mod 2 = 0), []); expected = [] };
          { input = ((fun x -> x mod 2 = 0), [0; 2; 4; 6; 8]); expected = [0; 2; 4; 6; 8] };
          { input = ((fun x -> x mod 2 = 0), [1; 3; 5; 7; 9]); expected = [] };
          { input = ((fun _ -> true), [1; 2; 3; 4; 5]); expected = [1; 2; 3; 4; 5] };
          { input = ((fun _ -> false), [1; 2; 3; 4; 5]); expected = [] };
      ]
})
