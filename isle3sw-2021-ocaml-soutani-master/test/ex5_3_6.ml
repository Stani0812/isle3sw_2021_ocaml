open Tester
module T = Testtools
module Ex = Exercise.Main


let () = ignore (run_test_case {
      name  = "Ex.5.3(6)";
      func  = Ex.belong;
      prep  = T.uncurry2;
      ishow = T.str_pair string_of_int (T.str_list string_of_int);
      oshow = string_of_bool;
      cmp   = (=);
      dataset = [
          { input = (10, []); expected = false };
          { input = (10, [10]); expected = true };
          { input = (10, [3]); expected = false };
          { input = (4, [4; 1; 2; 7; 2]); expected = true };
          { input = (3, [1; 2; 3]); expected = true };
          { input = (3, [1; 2; 4; 5]); expected = false };
          { input = (3, [1; 2; 4; 5; 0; 3]); expected = true };
      ]
} |> ignore; run_test_case {
      name  = "Ex.5.3(6)";
      func  = Ex.intersect;
      prep  = T.uncurry2;
      ishow = T.str_pair (T.str_list string_of_int) (T.str_list string_of_int);
      oshow = T.str_list string_of_int;
      cmp   = (fun x y -> List.sort compare x = List.sort compare y);
      dataset = [
          { input = ([], []); expected = [] };
          { input = ([], [1]); expected = [] };
          { input = ([1], []); expected = [] };
          { input = ([1; 3; 5; 7], [2; 4; 6]); expected = [] };
          { input = ([1; 7; 2; 3; 5; 8], [5; 0; 9; 7; 8; 1]); expected = [5; 7; 8; 1] };
          { input = ([1; 7; 2; 3; 5; 8], [0; 5; 9; 7; 8; 1; 4]); expected = [5; 7; 8; 1] };
      ]
} |> ignore; run_test_case {
      name  = "Ex.5.3(6)";
      func  = Ex.union;
      prep  = T.uncurry2;
      ishow = T.str_pair (T.str_list string_of_int) (T.str_list string_of_int);
      oshow = T.str_list string_of_int;
      cmp   = (fun x y -> List.sort compare x = List.sort compare y);
      dataset = [
          { input = ([], []); expected = [] };
          { input = ([], [1]); expected = [1] };
          { input = ([1], []); expected = [1] };
          { input = ([1; 3; 5; 7], [2; 4; 6]); expected = [1; 2; 3; 4; 5; 6; 7] };
          { input = ([1; 7; 2; 3; 5; 8], [5; 0; 9; 7; 8; 1]); expected = [0; 1; 2; 3; 5; 7; 8; 9] };
          { input = ([1; 7; 2; 3; 5; 8], [0; 5; 9; 7; 8; 1; 4]); expected = [0; 1; 2; 3; 4; 5; 7; 8; 9] };
      ]
} |> ignore; run_test_case {
      name  = "Ex.5.3(6)";
      func  = Ex.diff;
      prep  = T.uncurry2;
      ishow = T.str_pair (T.str_list string_of_int) (T.str_list string_of_int);
      oshow = T.str_list string_of_int;
      cmp   = (fun x y -> List.sort compare x = List.sort compare y);
      dataset = [
          { input = ([], []); expected = [] };
          { input = ([], [1]); expected = [] };
          { input = ([1], []); expected = [1] };
          { input = ([1; 3; 5; 7], [2; 4; 6]); expected = [1; 3; 5; 7] };
          { input = ([1; 7; 2; 3; 5; 8], [5; 0; 9; 7; 8; 1]); expected = [2; 3] };
          { input = ([1; 7; 2; 3; 5; 8], [0; 5; 9; 7; 8; 1; 4]); expected = [2; 3] };
      ]
})

