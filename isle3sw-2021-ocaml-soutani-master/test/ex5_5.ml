open Tester
module T = Testtools
module Ex = Exercise.Main


let () = ignore (run_test_case {
      name  = "Ex.5.5";
      func  = Ex.forall;
      prep  = (fun f x -> f (not) x);
      ishow = T.str_list string_of_bool;
      oshow = string_of_bool;
      cmp   = (=);
      dataset = [
          { input = []; expected = true };
          { input = [true]; expected = false };
          { input = [false]; expected = true };
          { input = [true; false]; expected = false };
          { input = [false; true]; expected = false };
          { input = [true; true]; expected = false };
          { input = [false; false]; expected = true };
          { input = [true; true; false]; expected = false };
          { input = [true; false; true]; expected = false };
          { input = [false; true; true]; expected = false };
          { input = [false; false; true]; expected = false };
          { input = [false; true; false]; expected = false };
          { input = [true; false; false]; expected = false };
      ]
} |> ignore; run_test_case {
      name  = "Ex.5.5";
      func  = Ex.forall;
      prep  = (fun f x -> f (fun x -> x mod 2 = 0) x);
      ishow = T.str_list string_of_int;
      oshow = string_of_bool;
      cmp   = (=);
      dataset = [
          { input = []; expected = true };
          { input = [0]; expected = true };
          { input = [1]; expected = false };
          { input = [2; 3]; expected = false };
          { input = [5; 6]; expected = false };
          { input = [1; 3]; expected = false };
          { input = [2; 4]; expected = true };
      ]
} |> ignore; run_test_case {
      name  = "Ex.5.5";
      func  = Ex.exists;
      prep  = (fun f x -> f (not) x);
      ishow = T.str_list string_of_bool;
      oshow = string_of_bool;
      cmp   = (=);
      dataset = [
          { input = []; expected = false };
          { input = [true]; expected = false };
          { input = [false]; expected = true };
          { input = [true; false]; expected = true };
          { input = [false; true]; expected = true };
          { input = [true; true]; expected = false };
          { input = [false; false]; expected = true };
          { input = [true; true; false]; expected = true };
          { input = [true; false; true]; expected = true };
          { input = [false; true; true]; expected = true };
          { input = [false; false; true]; expected = true };
          { input = [false; true; false]; expected = true };
          { input = [true; false; false]; expected = true };
      ]
} |> ignore; run_test_case {
      name  = "Ex.5.5";
      func  = Ex.exists;
      prep  = (fun f x -> f (fun x -> x mod 2 = 0) x);
      ishow = T.str_list string_of_int;
      oshow = string_of_bool;
      cmp   = (=);
      dataset = [
          { input = []; expected = false };
          { input = [0]; expected = true };
          { input = [1]; expected = false };
          { input = [2; 3]; expected = true };
          { input = [5; 6]; expected = true };
          { input = [1; 3]; expected = false };
          { input = [2; 4]; expected = true };
      ]
})

