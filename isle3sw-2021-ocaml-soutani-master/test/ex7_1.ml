open Tester
module T = Testtools
module Ex = Exercise.Main


let string_of_ref f x = "{ contents = " ^ f (Ex.(!) x) ^ " }"

let _ = run_test_case {
      name  = "Ex.7.1";
      func  = (fun r -> Ex.ref r);
      prep  = T.id;
      ishow = string_of_int;
      oshow = string_of_ref string_of_int;
      cmp   = (=);
      dataset = [
        { input = 0; expected = Ex.{ contents = 0 } };
        { input = 1; expected = Ex.{ contents = 1 } };
        { input = 334; expected = Ex.{ contents = 334 } };
      ]
} |> ignore; run_test_case {
    name  = "Ex.7.1";
    func  = (fun r -> Ex.ref r);
    prep  = T.id;
    ishow = T.id;
    oshow = string_of_ref T.id;
    cmp   = (=);
    dataset = [
      { input = "Hello, World!"; expected = Ex.{ contents = "Hello, World!" } };
    ]
} |> ignore; run_test_case {
    name  = "Ex.7.1";
    func  = Ex.(!);
    prep  = T.id;
    ishow = string_of_ref T.id;
    oshow = T.id;
    cmp   = (=);
    dataset = [
      { input = Ex.{ contents = "Hello, World!" }; expected = "Hello, World!" };
    ]
} |> ignore; run_test_case {
    name  = "Ex.7.1";
    func  = (fun (x, y) -> Ex.(:=) x y; Ex.(!) x);
    prep  = T.id;
    ishow = T.str_pair (string_of_ref T.id) T.id;
    oshow = T.id;
    cmp   = (=);
    dataset = [
      { input = (Ex.{ contents = "hoge" }, "fuga"); expected = "fuga" };
    ]
}
