open Tester
module T = Testtools
module Ex = Exercise.Main


let () = ignore (run_test_case {
			name  = "Ex.5.2";
			func  = Ex.sum_list;
			prep  = T.id;
			ishow = T.str_list string_of_int;
			oshow = string_of_int;
			cmp   = (=);
      dataset = [
					{ input = []; expected = 0 };
					{ input = [10]; expected = 10 };
					{ input = [1; 2; 3; 2; 1; 5]; expected = 14 };
      ]
} |> ignore; run_test_case {
			name  = "Ex.5.2";
			func  = Ex.max_list;
			prep  = T.id;
			ishow = T.str_list string_of_int;
			oshow = string_of_int;
			cmp   = (=);
      dataset = [
				{ input = [1]; expected = 1 };
				{ input = [2; 3; 10; 20; 5]; expected = 20 };
				{ input = [10; 1]; expected = 10 };
				{ input = [1; 10]; expected = 10 };
				{ input = [-8; -2; -10; -99]; expected = -2 };
				{ input = [-4611686018427387874; -4611686018427387904; -4611686018427387894]; expected = -4611686018427387874 };
      ]
})
