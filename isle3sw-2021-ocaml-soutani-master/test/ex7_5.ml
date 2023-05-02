open Tester
module T = Testtools
module Ex = Exercise.Main


let () = ignore (run_test_case {
      name  = "Ex.7.5";
      func  = Ex.fact_safety;
      prep  = (fun f x -> try Some (f x) with Invalid_argument _ -> None);
      ishow = string_of_int;
      oshow = T.str_option string_of_int;
      cmp   = (=);
      dataset = [
          { input = 0; expected = Some 1 };
          { input = 1; expected = Some 1 };
          { input = 2; expected = Some 2 };
          { input = 3; expected = Some 6 };
          { input = 4; expected = Some 24 };
          { input = 5; expected = Some 120 };
          { input = 6; expected = Some 720 };
          { input = 10; expected = Some 3628800 };
          { input = -1; expected = None };
          { input = -2; expected = None };
          { input = -3; expected = None };
          { input = -10; expected = None };
      ]
})
