open Tester
module T = Testtools
module Ex = Exercise.Main

let rec string_of_tree to_str = function
  | Ex.Lf -> "Lf"
  | Ex.Br (x, l, r) -> "Br(" ^ to_str x ^ "," ^ string_of_tree to_str l ^ "," ^ string_of_tree to_str r ^ ")"

let lf = Ex.Lf
let br (x,l,r) = Ex.Br (x, l, r)

let () = ignore (run_test_case {
      name  = "Ex.6.6";
      func  = Ex.reflect;
      prep  = T.id;
      ishow = string_of_tree string_of_int;
      oshow = string_of_tree string_of_int;
      cmp   = (=);
      dataset = [
          { input = lf; expected = lf };
          { input = br(28,lf,lf); expected = br(28,lf,lf) };
          { input = br(91,lf,br(13,lf,lf)); expected = br(91,br(13,lf,lf),lf) };
          { input = br(65,lf,br(29,lf,br(16,lf,lf))); expected = br(65,br(29,br(16,lf,lf),lf),lf) };
          { input = br(63,lf,br(48,lf,br(88,br(1,lf,lf),lf))); expected = br(63,br(48,br(88,lf,br(1,lf,lf)),lf),lf) };
          { input = br(24,lf,br(29,br(95,lf,lf),br(4,br(59,lf,lf),lf))); expected = br(24,br(29,br(4,lf,br(59,lf,lf)),br(95,lf,lf)),lf) };
          { input = br(27,br(82,br(96,br(44,lf,lf),lf),lf),br(40,br(84,lf,lf),lf)); expected = br(27,br(40,lf,br(84,lf,lf)),br(82,lf,br(96,lf,br(44,lf,lf)))) };
          { input = br(57,lf,br(45,br(6,lf,lf),br(39,lf,br(77,br(59,lf,br(65,lf,lf)),lf)))); expected = br(57,br(45,br(39,br(77,lf,br(59,br(65,lf,lf),lf)),lf),br(6,lf,lf)),lf) };
          { input = br(72,br(65,br(35,lf,br(87,lf,lf)),lf),br(70,br(88,lf,lf),br(67,br(47,lf,lf),lf))); expected = br(72,br(70,br(67,lf,br(47,lf,lf)),br(88,lf,lf)),br(65,lf,br(35,br(87,lf,lf),lf))) };
          { input = br(98,lf,br(81,br(5,br(92,br(74,lf,br(31,lf,lf)),br(18,lf,lf)),lf),br(82,lf,br(33,lf,lf)))); expected = br(98,br(81,br(82,br(33,lf,lf),lf),br(5,lf,br(92,br(18,lf,lf),br(74,br(31,lf,lf),lf)))),lf) };
      ]
})
