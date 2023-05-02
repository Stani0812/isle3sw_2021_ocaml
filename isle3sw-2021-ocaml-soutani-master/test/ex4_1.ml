open Tester
module T = Testtools
module Ex = Exercise.Main

let () = ignore (run_test_case {
      name  = "Ex.4.1";
      func  = Ex.integral;
      prep  = T.uncurry3;
      ishow = T.str_triple T.str_fun string_of_float string_of_float;
      oshow = string_of_float;
      cmp   = T.eq_float;
      dataset = [
          { input = ((fun x -> x), 0.0, 10.0); expected = 50.0000000000 };
          { input = ((fun x -> x), -12.89, 37.17); expected = 607.7284000000 };
          { input = ((fun x -> x *. x +. 5.4), -1.79, 9.86); expected = 384.3501983597 };
          { input = ((fun x -> sin x), 0.0, 3.141592653589); expected = 1.9999999998 };
      ]
})
