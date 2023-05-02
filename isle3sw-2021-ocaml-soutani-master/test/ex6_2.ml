open Tester
module T = Testtools
module Ex = Exercise.Main


let rec string_of_nat = function
  | Ex.Zero -> "Zero"
  | Ex.OneMoreThan n -> "OneMoreThan" ^ "(" ^ string_of_nat n ^ ")"

let o = Ex.Zero
let s n = Ex.OneMoreThan n

let () = ignore (run_test_case {
      name  = "Ex.6.2";
      func  = Ex.int_of_nat;
      prep  = T.id;
      ishow = string_of_nat;
      oshow = string_of_int;
      cmp   = (=);
      dataset = [
        { input = o; expected = 0 };
        { input = s o; expected = 1 };
        { input = s(s(o)); expected = 2 };
        { input = s(s(s(o))); expected = 3 };
        { input = s(s(s(s(s(o))))); expected = 5 };
        { input = s(s(s(s(s(s(s(o))))))); expected = 7 };
        { input = s(s(s(s(s(s(s(s(s(s(o)))))))))); expected = 10 };
      ]
} |> ignore; run_test_case {
      name  = "Ex.6.2";
      func  = Ex.mul;
      prep  = T.uncurry2;
      ishow = T.str_pair string_of_nat string_of_nat;
      oshow = string_of_nat;
      cmp   = (=);
      dataset = [
          { input = (o, o); expected = o };
          { input = (o, s(o)); expected = o };
          { input = (o, s(s(s(o)))); expected = o };
          { input = (s(o), o); expected = o };
          { input = (s(s(s(o))), o); expected = o };
          { input = (s(o), s(o)); expected = s(o) };
          { input = (s(o), s(s(s(s(s(o)))))); expected = s(s(s(s(s(o))))) };
          { input = (s(s(s(s(s(o))))), s(o)); expected = s(s(s(s(s(o))))) };
          { input = (s(s(s(o))), s(s(s(s(o))))); expected = s(s(s(s(s(s(s(s(s(s(s(s(o)))))))))))) };
          { input = (s(s(s(s(s(s(s(o))))))), s(s(s(s(o))))); expected = s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(o)))))))))))))))))))))))))))) };
      ]
} |> ignore; run_test_case {
      name  = "Ex.6.2";
      func  = Ex.monus;
      prep  = T.uncurry2;
      ishow = T.str_pair string_of_nat string_of_nat;
      oshow = string_of_nat;
      cmp   = (=);
      dataset = [
          { input = (o, o); expected = o };
          { input = (o, s(o)); expected = o };
          { input = (o, s(s(s(o)))); expected = o };
          { input = (s(o), o); expected = s(o) };
          { input = (s(s(s(o))), o); expected = s(s(s(o))) };
          { input = (s(o), s(o)); expected = o };
          { input = (s(o), s(s(s(s(s(o)))))); expected = o };
          { input = (s(s(s(s(s(o))))), s(o)); expected = s(s(s(s(o)))) };
          { input = (s(s(s(o))), s(s(s(s(o))))); expected = o };
          { input = (s(s(s(s(s(s(s(o))))))), s(s(s(s(o))))); expected = s(s(s(o))) };
      ]
})
