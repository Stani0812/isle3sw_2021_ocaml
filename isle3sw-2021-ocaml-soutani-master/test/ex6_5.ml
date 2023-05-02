open Tester
module T = Testtools
module Ex = Exercise.Main

let rec string_of_tree to_str = function
  | Ex.Lf -> "Lf"
  | Ex.Br (x, l, r) -> "Br(" ^ to_str x ^ "," ^ string_of_tree to_str l ^ "," ^ string_of_tree to_str r ^ ")"

let lf = Ex.Lf
let br (x,l,r) = Ex.Br (x, l, r)

let () = ignore (run_test_case {
      name  = "Ex.6.5";
      func  = Ex.inord;
      prep  = T.id;
      ishow = string_of_tree string_of_int;
      oshow = T.str_list string_of_int;
      cmp   = (=);
      dataset = [
          { input = lf; expected = [] };
          { input = br(80,lf,lf); expected = [80] };
          { input = br(49,lf,br(47,lf,lf)); expected = [49; 47] };
          { input = br(40,br(65,lf,lf),br(36,lf,lf)); expected = [65; 40; 36] };
          { input = br(69,br(97,lf,lf),br(64,lf,br(4,lf,lf))); expected = [97; 69; 64; 4] };
          { input = br(45,br(33,lf,lf),br(61,br(78,lf,lf),br(12,lf,lf))); expected = [33; 45; 78; 61; 12] };
          { input = br(41,br(22,br(57,br(60,lf,br(1,lf,lf)),lf),lf),br(72,lf,lf)); expected = [60; 1; 57; 22; 41; 72] };
          { input = br(17,br(69,lf,lf),br(99,br(34,lf,br(84,br(8,lf,lf),lf)),br(58,lf,lf))); expected = [69; 17; 34; 8; 84; 99; 58] };
          { input = br(63,br(15,lf,br(34,lf,lf)),br(81,br(24,br(0,lf,br(54,lf,lf)),lf),br(37,lf,lf))); expected = [15; 34; 63; 0; 54; 24; 81; 37] };
          { input = br(33,br(42,lf,br(52,lf,br(34,br(52,lf,br(47,lf,lf)),br(36,lf,br(13,lf,lf))))),br(41,lf,lf)); expected = [42; 52; 52; 47; 34; 36; 13; 33; 41] };
      ]
} |> ignore; run_test_case {
      name  = "Ex.6.5";
      func  = Ex.postord;
      prep  = T.id;
      ishow = string_of_tree string_of_int;
      oshow = T.str_list string_of_int;
      cmp   = (=);
      dataset = [
          { input = lf; expected = [] };
          { input = br(48,lf,lf); expected = [48] };
          { input = br(58,br(65,lf,lf),lf); expected = [65; 58] };
          { input = br(16,br(64,lf,lf),br(60,lf,lf)); expected = [64; 60; 16] };
          { input = br(63,br(0,lf,br(95,lf,lf)),br(8,lf,lf)); expected = [95; 0; 8; 63] };
          { input = br(89,br(33,br(81,lf,br(70,br(78,lf,lf),lf)),lf),lf); expected = [78; 70; 81; 33; 89] };
          { input = br(53,br(81,br(37,lf,br(35,lf,lf)),lf),br(41,br(68,lf,lf),lf)); expected = [35; 37; 81; 68; 41; 53] };
          { input = br(76,br(27,br(31,lf,br(54,lf,lf)),lf),br(92,lf,br(65,lf,br(63,lf,lf)))); expected = [54; 31; 27; 63; 65; 92; 76] };
          { input = br(39,br(16,br(46,br(89,br(85,lf,lf),lf),lf),br(47,br(54,lf,lf),lf)),br(68,lf,lf)); expected = [85; 89; 46; 54; 47; 16; 68; 39] };
          { input = br(57,br(95,br(23,br(71,lf,lf),lf),br(60,lf,lf)),br(16,br(70,br(48,lf,lf),br(15,lf,lf)),lf)); expected = [71; 23; 60; 95; 48; 15; 70; 16; 57] };
      ]
})
