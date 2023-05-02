open Tester
module T = Testtools
module Ex = Exercise.Main


let () = ignore (run_test_case {
      name  = "Ex.5.3(2)";
      func  = Ex.roman;
      prep  = T.uncurry2;
      ishow = T.str_pair (T.str_list (T.str_pair string_of_int T.id)) string_of_int;
      oshow = T.id;
      cmp   = (=);
      dataset = [
          { input = ([(1000, "M"); (500,"D"); (100, "C"); (50, "L"); (10, "X"); (5, "V"); (1, "I")], 1984); expected = "MDCCCCLXXXIIII" };
          { input = ([(1000,"M"); (900, "CM"); (500,"D"); (400, "CD"); (100, "C"); (90, "XC"); (50, "L"); (40, "XL"); (10, "X"); (9, "IX"); (5, "V"); (4, "IV"); (1, "I")], 1984); expected = "MCMLXXXIV" };
          { input = ([(10000000000000000, "X"); (1, "I")], 100000000000000000); expected = "XXXXXXXXXX" };
          { input = ([(100, "C"); (25, "Q"); (5, "V"); (1, "I")], 183); expected = "CQQQVIII" };
      ]
})
