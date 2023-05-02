open Tester
module T = Testtools
module Ex = Exercise.Main

let () = ignore (run_test_case {
      name  = "Ex.2.6(3)";
      func   = Ex.yen_of_usdollar_message;
      prep  = T.id;
      ishow = string_of_float;
      oshow = T.id;
      cmp  = (=);
      dataset = [
          { input = 0.; expected = "0. dollars are 0 yen." };
          { input = 10.; expected = "10. dollars are 1111 yen." };
          { input = 40.; expected = "40. dollars are 4445 yen." };
          { input = 117.; expected = "117. dollars are 13001 yen." };
          { input = 259.; expected = "259. dollars are 28780 yen." };
          { input = 712.; expected = "712. dollars are 79117 yen." };
          { input = 5000.; expected = "5000. dollars are 555600 yen." };
          { input = 6.24; expected = "6.24 dollars are 693 yen." };
          { input = 6.25; expected = "6.25 dollars are 695 yen." };
          { input = 6.26; expected = "6.26 dollars are 696 yen." };
          { input = 22.04; expected = "22.04 dollars are 2449 yen." };
          { input = 58.63; expected = "58.63 dollars are 6515 yen." };
          { input = 72.33; expected = "72.33 dollars are 8037 yen." };
          { input = 48.72; expected = "48.72 dollars are 5414 yen." };
          { input = 49.38; expected = "49.38 dollars are 5487 yen." };
          { input = 83.23; expected = "83.23 dollars are 9249 yen." };
          { input = 92.78; expected = "92.78 dollars are 10310 yen." };
          { input = 71.15; expected = "71.15 dollars are 7906 yen." };
          { input = 83.67; expected = "83.67 dollars are 9297 yen." };
          { input = 32.75; expected = "32.75 dollars are 3639 yen." };
          { input = 11.78; expected = "11.78 dollars are 1309 yen." };
          { input = 80.74; expected = "80.74 dollars are 8972 yen." };
          { input = 7.5; expected = "7.5 dollars are 833 yen." };
          { input = 0.8; expected = "0.8 dollars are 89 yen." };
          { input = 78.66; expected = "78.66 dollars are 8741 yen." };
          { input = 43.19; expected = "43.19 dollars are 4799 yen." };
          { input = 49.27; expected = "49.27 dollars are 5475 yen." };
          { input = 45.67; expected = "45.67 dollars are 5075 yen." };
          { input = 21.68; expected = "21.68 dollars are 2409 yen." };
          { input = 71.49; expected = "71.49 dollars are 7944 yen." };
          { input = 69.79; expected = "69.79 dollars are 7755 yen." };
          { input = 33.65; expected = "33.65 dollars are 3739 yen." };
          { input = 55.32; expected = "55.32 dollars are 6147 yen." };
          { input = 11.76; expected = "11.76 dollars are 1307 yen." };
          { input = 56.08; expected = "56.08 dollars are 6232 yen." };
          { input = 64.9; expected = "64.9 dollars are 7212 yen." };
          { input = 8.25; expected = "8.25 dollars are 917 yen." };
          { input = 58.17; expected = "58.17 dollars are 6464 yen." };
          { input = 30.7; expected = "30.7 dollars are 3411 yen." };
          { input = 23.59; expected = "23.59 dollars are 2621 yen." };
          { input = 22.98; expected = "22.98 dollars are 2554 yen." };
          { input = 7.4; expected = "7.4 dollars are 822 yen." };
          { input = 58.89; expected = "58.89 dollars are 6544 yen." };
          { input = 84.81; expected = "84.81 dollars are 9424 yen." };
          { input = 92.22; expected = "92.22 dollars are 10247 yen." };
          { input = 50.03; expected = "50.03 dollars are 5559 yen." };
          { input = 31.69; expected = "31.69 dollars are 3521 yen." };
          { input = 75.68; expected = "75.68 dollars are 8410 yen." };
          { input = 82.82; expected = "82.82 dollars are 9203 yen." };
          { input = 77.25; expected = "77.25 dollars are 8584 yen." };
      ]
})
