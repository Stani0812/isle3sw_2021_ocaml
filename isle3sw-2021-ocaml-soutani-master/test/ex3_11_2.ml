open Tester
module T = Testtools
module Ex = Exercise.Main

let () = ignore (run_test_case {
      name  = "Ex.3.11(2)";
      func  = Ex.comb;
      prep  = T.id;
      ishow = (fun (x, y) -> Printf.sprintf "(%d, %d)" x y);
      oshow = string_of_int;
      cmp   = (=);
      dataset = [
          { input = (1, 0); expected = 1 };
          { input = (1, 1); expected = 1 };
          { input = (10, 0); expected = 1 };
          { input = (10, 4); expected = 210 };
          { input = (10, 8); expected = 45 };
          { input = (10, 10); expected = 1 };
          { input = (20, 0); expected = 1 };
          { input = (20, 1); expected = 20 };
          { input = (20, 5); expected = 15504 };
          { input = (20, 7); expected = 77520 };
          { input = (20, 10); expected = 184756 };
          { input = (20, 13); expected = 77520 };
          { input = (20, 15); expected = 15504 };
          { input = (20, 19); expected = 20 };
          { input = (20, 20); expected = 1 };
      ]
})
