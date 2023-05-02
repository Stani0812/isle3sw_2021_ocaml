open OUnit
open Testutil

type ('a, 'b, 'f) test =
  { name    : string;
    func    : 'f;
    prep    : 'f -> 'a -> 'b;
    ishow   : 'a -> string;
    oshow   : 'b -> string;
    cmp    : 'b -> 'b -> bool;
    dataset : ('a, 'b) testcase list
  }

let run_test_case (case: ('a, 'b, 'f) test) = run_test_tt_main (
    case.name >:::
    gen_tests
      ~cmp: case.cmp
      ~ishow: case.ishow
      ~oshow: case.oshow
      ~exec: (case.prep case.func)
      case.dataset
  )
