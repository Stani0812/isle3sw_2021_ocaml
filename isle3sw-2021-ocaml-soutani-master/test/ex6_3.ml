open Tester
module T = Testtools
module Ex = Exercise.Main

let rec string_of_nat = function
  | Ex.Zero -> "Zero"
  | Ex.OneMoreThan n -> "OneMoreThan" ^ "(" ^ string_of_nat n ^ ")"

let o = Ex.Zero
let s n = Ex.OneMoreThan n

let () = ignore (run_test_case {
      name  = "Ex.6.3";
      func  = Ex.minus;
      prep  = T.uncurry2;
      ishow = T.str_pair string_of_nat string_of_nat;
      oshow = T.str_option string_of_nat;
      cmp   = (=);
      dataset = [
          { input = (o, o); expected = Some o };
          { input = (o, s(o)); expected = None };
          { input = (o, s(s(s(o)))); expected = None };
          { input = (s(o), o); expected = Some (s(o)) };
          { input = (s(s(s(o))), o); expected = Some (s(s(s(o)))) };
          { input = (s(o), s(o)); expected = Some o };
          { input = (s(o), s(s(s(s(s(o)))))); expected = None };
          { input = (s(s(s(s(s(o))))), s(o)); expected = Some (s(s(s(s(o))))) };
          { input = (s(s(s(o))), s(s(s(s(o))))); expected = None };
          { input = (s(s(s(s(s(s(s(o))))))), s(s(s(s(o))))); expected = Some (s(s(s(o)))) };
      ]
})
