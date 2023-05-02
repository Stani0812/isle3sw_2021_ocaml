open OUnit

type ('a, 'b) testcase = { input: 'a; expected: 'b }

let gen_tests ?(cmp=(=)) ~ishow ~oshow ~exec tests =
  List.map
    (fun (test: ('a, 'b) testcase) ->
       "" >:: fun () ->
         let actual = exec test.input in
         assert_bool
           (Printf.sprintf
              "input:     '%s'\nactual:    %s\nexpected:  %s\n"
              (ishow test.input)
              (oshow actual)
              (oshow test.expected))
           (cmp actual test.expected)
    )
    tests
