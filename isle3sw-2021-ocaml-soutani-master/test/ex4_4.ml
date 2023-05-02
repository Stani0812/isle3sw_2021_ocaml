open Tester
module T = Testtools
module Ex = Exercise.Main

let () = ignore (run_test_case {
			name  = "Ex.4.4";
			func  = Ex.uncurry;
			prep  = T.uncurry2;
			ishow = T.str_pair T.str_fun (T.str_pair T.id T.id);
			oshow = T.id;
			cmp   = (=);
      dataset = [
          { input = (max, ("abc", "cba")); expected = "cba" };
          { input = (max, ("xyz", "cba")); expected = "xyz" };
          { input = ((^), ("abc", "cba")); expected = "abccba" };
      ]
})
