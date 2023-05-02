open Tester
module T = Testtools
module Ex = Exercise.Main

let () = ignore (run_test_case {
      name  = "Ex.3.3";
      func  = Ex.geo_mean;
      prep  = T.id;
      ishow = (fun (x, y) -> Printf.sprintf "(%f, %f)" x y);
      oshow = T.string_of_float10;
      cmp  = T.eq_float;
      dataset = [
          { input = (1., 2.); expected = 1.4142135624 };
          { input = (3.14159, 2.71828); expected = 2.9222801483 };
      ]
})
