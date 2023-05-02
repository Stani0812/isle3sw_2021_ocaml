open Tester
module T = Testtools
module Ex = Exercise.Main

type change_result = Ok of int list | NG | Error

let () = ignore (run_test_case {
      name  = "Ex.7.8";
      func  = Ex.change;
      prep  = (fun f x -> try Ok (f x) with Failure mes -> if mes = "change" then NG else Error);
      ishow = T.str_pair (T.str_list string_of_int) string_of_int;
      oshow = (function
                | Ok x -> T.str_list string_of_int x
                | NG -> "Failure change"
                | Error -> "Other exception" );
      cmp   = (=);
      dataset = [
          { input = ([5; 2], 16); expected = Ok [5; 5; 2; 2; 2] };
          { input = ([20; 8], 19); expected = NG };
          { input = ([5; 2], 1); expected = NG };
          { input = ([5; 2], 6); expected = Ok [2; 2; 2] };
          { input = ([49; 23; 13], 292); expected = Ok [49; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 13] };
      ]
})
