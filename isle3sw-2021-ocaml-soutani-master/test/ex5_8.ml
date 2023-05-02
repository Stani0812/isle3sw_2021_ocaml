open Tester
module T = Testtools
module Ex = Exercise.Main


let () = ignore (run_test_case {
      name  = "Ex.5.8";
      func  = Ex.map2;
      prep  = (fun f x -> f (fun x -> x * 2 + 1) x);
      ishow = (fun x -> "<fun x -> x * 2 + 1>, " ^ T.str_list string_of_int x);
      oshow = T.str_list string_of_int;
      cmp   = (=);
      dataset = [
          { input = []; expected = [] };
          { input = [3; 1; 4; 1; 5; 9; 2]; expected = [7; 3; 9; 3; 11; 19; 5] };
      ]
} |> ignore; run_test_case {
      name  = "Ex.5.8";
      func  = Ex.map2;
      prep  = (fun f x -> f String.length x);
      ishow = (fun x -> "String.length, " ^ T.str_list T.id x);
      oshow = T.str_list string_of_int;
      cmp   = (=);
      dataset = [
            { input = []; expected = [] };
            { input = ["abc"; ""; "foovar"; "OCaml. Not Ocaml."]; expected = [3; 0; 6; 17] };
      ]
})
