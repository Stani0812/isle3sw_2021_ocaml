open Tester
module T = Testtools
module Ex = Exercise.Main

let () = ignore (run_test_case {
      name  = "Ex.3.11(4)";
      func  = Ex.max_ascii;
      prep  = T.id;
      ishow = T.id;
      oshow = Char.escaped;
      cmp   = (=);
      dataset = [
          { input = "a"; expected = 'a' };
          { input = " "; expected = ' ' };
          { input = "~"; expected = '~' };
          { input = "abc"; expected = 'c' };
          { input = "cab"; expected = 'c' };
          { input = "OCaml"; expected = 'm' };
          { input = "c7f!p(/"; expected = 'p' };
          { input = "2(_%x,a"; expected = 'x' };
          { input = "E-;jjDf"; expected = 'j' };
          { input = "syk`\"]t"; expected = 'y' };
          { input = "xMsHR|u"; expected = '|' };
          { input = "Z{.mu<f"; expected = '{' };
          { input = "{DU8z0N"; expected = '{' };
          { input = "B\"G)qKf"; expected = 'q' };
          { input = "o|(\"Mwb"; expected = '|' };
          { input = "oR5Dl3S"; expected = 'o' };
          { input = "UVz\"`i/"; expected = 'z' };
          { input = "^_o*'bu"; expected = 'u' };
          { input = "gj4Ain]"; expected = 'n' };
          { input = "R:rw72@"; expected = 'w' };
          { input = ";AcDAcp"; expected = 'p' };
          { input = "#WC4MMz"; expected = 'z' };
          { input = "!n<C_!X"; expected = 'n' };
          { input = "`&h;pD!"; expected = 'p' };
          { input = "~,+=OqK"; expected = '~' };
          { input = "yp r:Tz"; expected = 'z' };
          { input = "6~]Cj~,"; expected = '~' };
          { input = "CDK/c@&"; expected = 'c' };
          { input = "8y(g:F/"; expected = 'y' };
          { input = "5y\\KQ^M"; expected = 'y' };
          { input = "-9NW95B"; expected = 'W' };
          { input = "[c2ipqD"; expected = 'q' };
          { input = "Y2AgLMr"; expected = 'r' };
          { input = "S<Lo'Y["; expected = 'o' };
          { input = "J?T,CRB"; expected = 'T' };
          { input = "_LKt\\1t"; expected = 't' };
          { input = "Jr(=\\rj"; expected = 'r' };
          { input = ",%_}9rj"; expected = '}' };
          { input = "D_[%)| "; expected = '|' };
          { input = "O6.GFq%"; expected = 'q' };
          { input = "KXOi=bz"; expected = 'z' };
          { input = "D?%P~s."; expected = '~' };
          { input = "4H4PU i"; expected = 'i' };
          { input = "a7%qVro"; expected = 'r' };
          { input = "lv.OwMI"; expected = 'w' };
          { input = "X 996#|"; expected = '|' };
          { input = "gS4IM,c"; expected = 'g' };
          { input = "o2ebhP-"; expected = 'o' };
          { input = "l& E7q'"; expected = 'q' };
          { input = "9|w~yeR"; expected = '~' };
          { input = "\\2.6VYq"; expected = 'q' };
          { input = "LWW_G1S"; expected = '_' };
          { input = "CB7c/Lk"; expected = 'k' };
          { input = "fd9M >s"; expected = 's' };
          { input = "hqdJ\"yZ"; expected = 'y' };
          { input = "@_w:(59"; expected = 'w' };
      ]
})
