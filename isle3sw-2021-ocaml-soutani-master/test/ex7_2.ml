open Tester
module T = Testtools
module Ex = Exercise.Main

let ref = Ex.ref
let (!) = Ex.(!)

let _ = run_test_case {
      name  = "Ex.7.2";
      func  = (fun r -> let x = ref r in Ex.incr x; !x);
      prep  = T.id;
      ishow = string_of_int;
      oshow = string_of_int;
      cmp   = (=);
      dataset = [
          { input = 0; expected = 1 };
          { input = 1; expected = 2 };
          { input = 2; expected = 3 };
          { input = 3; expected = 4 };
          { input = -1; expected = 0 };
          { input = 9; expected = 10 };
      ]
}
