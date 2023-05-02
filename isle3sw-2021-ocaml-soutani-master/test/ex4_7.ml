open Tester
module T = Testtools
module Ex = Exercise.Main


let () = ignore (run_test_case {
			name  = "Ex.4.7";
			func  = Ex.second;
			prep  = T.uncurry2;
			ishow = (fun (x, y) -> Printf.sprintf "(%F, %s)" x y);
			oshow = T.id;
			cmp   = (=);
      dataset = [
					{ input = (1., "var"); expected = "var" };
      ]
} |> ignore; run_test_case {
			name  = "Ex.4.7";
			func  = Ex.second;
			prep  = T.uncurry2;
			ishow = (fun (x, y) -> Printf.sprintf "(%s, %d)" x y);
			oshow = string_of_int;
			cmp   = (=);
      dataset = [
					{ input = ("var", 4); expected = 4 };
      ]
})
