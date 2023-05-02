open Tester
module T = Testtools
module Ex = Exercise.Main

let rec string_of_tree to_str = function
  | Ex.Lf -> "Lf"
  | Ex.Br (x, l, r) -> "Br(" ^ to_str x ^ "," ^ string_of_tree to_str l ^ "," ^ string_of_tree to_str r ^ ")"

let lf = Ex.Lf
let br (x,l,r) = Ex.Br (x, l, r)

let () = ignore (run_test_case {
      name  = "Ex.6.4";
      func  = Ex.comptree;
      prep  = T.uncurry2;
      ishow = T.str_pair string_of_int string_of_int;
      oshow = string_of_tree string_of_int;
      cmp   = (=);
      dataset = [
          { input = (1, 0); expected = lf };
          { input = (0, 1); expected = br(0,lf,lf) };
          { input = (1, 2); expected = br(1,br(1,lf,lf),br(1,lf,lf)) };
          { input = (2, 3); expected = br(2,br(2,br(2,lf,lf),br(2,lf,lf)),br(2,br(2,lf,lf),br(2,lf,lf))) };
          { input = (10, 4); expected = br(10,br(10,br(10,br(10,lf,lf),br(10,lf,lf)),br(10,br(10,lf,lf),br(10,lf,lf))),br(10,br(10,br(10,lf,lf),br(10,lf,lf)),br(10,br(10,lf,lf),br(10,lf,lf)))) };
      ]
})
