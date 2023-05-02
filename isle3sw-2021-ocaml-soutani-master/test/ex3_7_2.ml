open Tester
module T = Testtools
module Ex = Exercise.Main

let () = ignore (run_test_case {
      name  = "Ex.3.7(2)";
      func  = Ex.pow2;
      prep  = T.id;
      ishow = (fun (x, y) -> Printf.sprintf "(%F, %d)" x y);
      oshow = T.string_of_float10;
      cmp   = T.eq_float;
      dataset = [
          { input = (0., 1); expected = 0.0000000000 };
          { input = (0., 1); expected = 0.0000000000 };
          { input = (0., 2); expected = 0.0000000000 };
          { input = (0., 3); expected = 0.0000000000 };
          { input = (0., 4); expected = 0.0000000000 };
          { input = (0., 5); expected = 0.0000000000 };
          { input = (0., 10); expected = 0.0000000000 };
          { input = (1., 1); expected = 1.0000000000 };
          { input = (1., 1); expected = 1.0000000000 };
          { input = (1., 2); expected = 1.0000000000 };
          { input = (1., 3); expected = 1.0000000000 };
          { input = (1., 4); expected = 1.0000000000 };
          { input = (1., 5); expected = 1.0000000000 };
          { input = (1., 10); expected = 1.0000000000 };
          { input = (2., 1); expected = 2.0000000000 };
          { input = (2., 1); expected = 2.0000000000 };
          { input = (2., 2); expected = 4.0000000000 };
          { input = (2., 3); expected = 8.0000000000 };
          { input = (2., 4); expected = 16.0000000000 };
          { input = (2., 5); expected = 32.0000000000 };
          { input = (2., 10); expected = 1024.0000000000 };
          { input = (3.3, 1); expected = 3.3000000000 };
          { input = (3.3, 1); expected = 3.3000000000 };
          { input = (3.3, 2); expected = 10.8900000000 };
          { input = (3.3, 3); expected = 35.9370000000 };
          { input = (3.3, 4); expected = 118.5921000000 };
          { input = (3.3, 5); expected = 391.3539300000 };
          { input = (3.3, 10); expected = 153157.8985264448 };
          { input = (-3., 1); expected = -3.0000000000 };
          { input = (-3., 1); expected = -3.0000000000 };
          { input = (-3., 2); expected = 9.0000000000 };
          { input = (-3., 3); expected = -27.0000000000 };
          { input = (-3., 4); expected = 81.0000000000 };
          { input = (-3., 5); expected = -243.0000000000 };
          { input = (-3., 10); expected = 59049.0000000000 };
          { input = (1.2, 30); expected = 237.3763137998 };
          { input = (1.01, 1000); expected = 20959.1556378139 };
          { input = (1.0001, 100000); expected = 22015.4560484819 };
          { input = (0.999999999999, 1000000000000000000); expected = 0.0000000000 };
          { input = (1., 1000000000000000000); expected = 1.0000000000 };
          { input = (-1., 1000000000000000000); expected = 1.0000000000 };
          { input = (-1., 1000000000000000001); expected = -1.0000000000 };
      ]
})
