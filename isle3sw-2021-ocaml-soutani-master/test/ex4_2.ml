open Tester
module T = Testtools
module Ex = Exercise.Main


let _ = run_test_case {
      name  = "Ex.4.2";
      func  = Ex.pow_curry;
      prep  = T.uncurry2;
      ishow = T.str_pair string_of_int T.string_of_float10;
      oshow = T.string_of_float10;
      cmp   = T.eq_float;
      dataset = [
        { input = (1, 0.); expected = 0.0000000000 };
        { input = (1, 0.); expected = 0.0000000000 };
        { input = (2, 0.); expected = 0.0000000000 };
        { input = (3, 0.); expected = 0.0000000000 };
        { input = (4, 0.); expected = 0.0000000000 };
        { input = (5, 0.); expected = 0.0000000000 };
        { input = (10, 0.); expected = 0.0000000000 };
        { input = (1, 1.); expected = 1.0000000000 };
        { input = (1, 1.); expected = 1.0000000000 };
        { input = (2, 1.); expected = 1.0000000000 };
        { input = (3, 1.); expected = 1.0000000000 };
        { input = (4, 1.); expected = 1.0000000000 };
        { input = (5, 1.); expected = 1.0000000000 };
        { input = (10, 1.); expected = 1.0000000000 };
        { input = (1, 2.); expected = 2.0000000000 };
        { input = (1, 2.); expected = 2.0000000000 };
        { input = (2, 2.); expected = 4.0000000000 };
        { input = (3, 2.); expected = 8.0000000000 };
        { input = (4, 2.); expected = 16.0000000000 };
        { input = (5, 2.); expected = 32.0000000000 };
        { input = (10, 2.); expected = 1024.0000000000 };
        { input = (1, 3.3); expected = 3.3000000000 };
        { input = (1, 3.3); expected = 3.3000000000 };
        { input = (2, 3.3); expected = 10.8900000000 };
        { input = (3, 3.3); expected = 35.9370000000 };
        { input = (4, 3.3); expected = 118.5921000000 };
        { input = (5, 3.3); expected = 391.3539300000 };
        { input = (10, 3.3); expected = 153157.8985264448 };
        { input = (1, -3.); expected = -3.0000000000 };
        { input = (1, -3.); expected = -3.0000000000 };
        { input = (2, -3.); expected = 9.0000000000 };
        { input = (3, -3.); expected = -27.0000000000 };
        { input = (4, -3.); expected = 81.0000000000 };
        { input = (5, -3.); expected = -243.0000000000 };
        { input = (10, -3.); expected = 59049.0000000000 };
        { input = (30, 1.2); expected = 237.3763137998 };
        { input = (1000, 1.01); expected = 20959.1556378139 };
        { input = (100000, 1.0001); expected = 22015.4560484819 };
      ]
} |> ignore; run_test_case {
      name  = "Ex.4.2";
      func  = Ex.cube;
      prep  = T.id;
      ishow = T.string_of_float10;
      oshow = T.string_of_float10;
      cmp   = T.eq_float;
      dataset = [
        { input = 0.; expected = 0. };
        { input = 1.; expected = 1. };
        { input = 2.; expected = 8. };
        { input = 0.0319154205; expected = 0.0000325089 };
        { input = 0.6955763031; expected = 0.3365381741 };
        { input = 0.0414097226; expected = 0.0000710079 };
        { input = 0.6506528412; expected = 0.2754533076 };
        { input = 0.9235389388; expected = 0.7877086842 };
        { input = 0.9098345041; expected = 0.7531599334 };
        { input = 0.7112154790; expected = 0.3597523175 };
        { input = 0.0718125889; expected = 0.0003703410 };
        { input = 0.2491208225; expected = 0.0154607333 };
        { input = 0.8659924276; expected = 0.6494448593 };
        { input = 0.2645899279; expected = 0.0185233667 };
        { input = 0.2425009178; expected = 0.0142606776 };
        { input = 0.2146644170; expected = 0.0098919106 };
        { input = 0.0999222240; expected = 0.0009976685 };
        { input = 0.3347371568; expected = 0.0375069517 };
        { input = 0.6522515203; expected = 0.2774886987 };
        { input = 0.1827469229; expected = 0.0061030962 };
        { input = 0.1328877550; expected = 0.0023466855 };
        { input = 0.6854189493; expected = 0.3220092302 };
        { input = 0.7529725849; expected = 0.4269111448 };
        { input = 0.6131756066; expected = 0.2305444163 };
        { input = 0.4783893129; expected = 0.1094824247 };
        { input = 0.1072251194; expected = 0.0012327915 };
        { input = 0.0913630249; expected = 0.0007626257 };
        { input = 0.5264741809; expected = 0.1459255143 };
        { input = 0.0057033522; expected = 0.0000001855 };
        { input = 0.5277045179; expected = 0.1469509633 };
        { input = 0.2556065683; expected = 0.0166999830 };
        { input = 0.6342436926; expected = 0.2551340781 };
        { input = 0.3037090111; expected = 0.0280138651 };
        { input = 0.3681642980; expected = 0.0499028115 };
        { input = 0.7798954167; expected = 0.4743611401 };
        { input = 0.0629244890; expected = 0.0002491490 };
        { input = 0.5138129502; expected = 0.1356485445 };
        { input = 0.5430179935; expected = 0.1601189237 };
        { input = 0.1465717221; expected = 0.0031488398 };
        { input = 0.8488017010; expected = 0.6115313467 };
        { input = 0.8217610759; expected = 0.5549280771 };
        { input = 0.2155203974; expected = 0.0100107159 };
        { input = 0.5913735510; expected = 0.2068167413 };
        { input = 0.9818524129; expected = 0.9465392668 };
        { input = 0.7863817399; expected = 0.4862955118 };
        { input = 0.9329497570; expected = 0.8120350362 };
        { input = 0.3791588246; expected = 0.0545084089 };
        { input = 0.2514544171; expected = 0.0158992928 };
        { input = 0.9315177848; expected = 0.8083016270 };
        { input = 0.2643013344; expected = 0.0184628214 };
        { input = 0.5357188363; expected = 0.1537484514 };
        { input = 0.4320655641; expected = 0.0806582811 };
        { input = 0.5584299011; expected = 0.1741429887 };
      ]
}
