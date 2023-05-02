(*
   Exercise 2.1
   1. 型：float　評価結果：5.5
   2. 型：int　評価結果：0
   3. 型：string　評価結果："foo"
   4. 型：char　評価結果：'U'
   5. 型：int　評価結果：255
   (なお、"0xff"の一番左は大文字のオーではなく、数字のゼロである。
   大文字のオーの場合は、Exception: Failure "int_of_string".と表示される。)
   6. 型：float　評価結果：25.
*)

(*
   Exercise 2.2
   1.予想：else以下の省略によりunit型とみなされているため、thenとelseの型が合わない。
   　実際：予想通り。
   2.予想：2つの演算子*と-が連続しているため、-2は括弧でくくる必要がある。
   　実際：予想通り。
   3.予想："0xfg"に対応するintは存在しない。
   　実際：概ね予想通り。0xは16進数を表すため、gは使用されない。
   4.予想：int_of_floatに負の値は適用できない。
   　実際：予想とは違った。-0.7を括弧でくくると適用できた。
*)

(*
   Exercise 2.3
   1. not (true && false)
   2. float_of_int(int_of_float 5.0)
   3. (sin(3.14/.2.0) ** 2.0)+.(cos(3.14/.2.0) ** 2.0)
   4. int_of_float(sqrt(float_of_int(3*3+4*4))
*)

(*
   Exercise 2.4
   <b1&&b2>
   if b1 then (if b2 then true else false) else false
   <b1||b2>
   if b1 then true else if b2 then true else false
*)

(*
   Exercise 2.5
   変数名の1文字目は英小文字か_しか使えないため、有効なのは、a_2',____,_'_'_,_の４つである。
   (修正)
    _ 一文字のみは除くため、有効なのは、a_2',____,_'_'_の3つである。
*)

(* Exercise 2.6 *)
let rounding ((data : float), (rank : int)) : float = (*ある位(小数第rank位)でdataを四捨五入*)
  if ((data *. (10.0**(float_of_int (rank-1)))) -. (floor (data *. (10.0**(float_of_int (rank-1)))))) < 0.5
  then floor (data *. (10.0**(float_of_int (rank-1)))) /. (10.0**(float_of_int (rank-1)))
  else (floor (data *. (10.0**(float_of_int (rank-1)))) +. 1.0) /. (10.0**(float_of_int (rank-1)));;
let yen_of_usdollar (usdollar : float) : int = 
  int_of_float (rounding ((usdollar *. 111.12), 1));;
let usdollar_of_yen (yen : int) : float =
  rounding (((float_of_int yen) /. 111.12), 3);;
let yen_of_usdollar_message (usdollar : float) : string =
  (string_of_float usdollar) ^ " dollars are " ^ (string_of_int (yen_of_usdollar usdollar)) ^ " yen.";;
let capitalize (oneChar : char) : char =
  if int_of_char oneChar >= 97
  then if int_of_char oneChar <= 122 then char_of_int ((int_of_char oneChar) - 32) else oneChar
  else oneChar;;

(*
   Exercise 3.1
   1. 予想：25 実際：25
   let x = 1 in let x = 3 in let x = x + 2 in x * x
   右辺におけるxは左から順に「let x = 3」「let x = x + 2」「let x = x + 2」である。
   2. 予想：13 実際：13
   let x = 2 and y = 3 in (let y = x and x = y + 2 in x * y) + y
   右辺におけるx、yは左から順に「let x = 2」「let y = 3」「let x = y + 2」「let y = x」「let y = 3」である。
   3. 予想：16 実際：16
   let x = 2 in let y = 3 in let y = x in let z = y + 2 in x * y * z
   右辺におけるx、y、zは左から順に「let x = 2」「let y = x」「let x = 2」「let y = x」「let z = y + 2」である。
*)

(*
   Exercise 3.2
   「let x = e1 and y = e2;;」と「let x = e1 let y = e2;;」について
   前者は、x = e1 と x = e2 を同時に評価するのに対して、後者は、x = e1 を評価してから x = e2 を評価する。
   すなわち、e2にxが含まれている場合、前者での y = e2 の評価ではまだxは更新されていないのに対して、後者での y = e2 の評価ではすでにxは更新されている。
*)

(* Exercise 3.3 *)
let geo_mean (dataA, dataB) = (*相乗平均*)
  sqrt (dataA *. dataB) ;;

(* 
   Exercise 3.4 
   let prodMatVec (((a_11, a_12), (a_21, a_22)), (vec1, vec2)) = 
     (((a_11 *. vec1) +. (a_12 *. vec2)), ((a_21 *. vec1) +. (a_22 *. vec2)));;
   ここでa_ijは、2行2列行列におけるi行j列の要素を表す。
   また、(2行2列の行列)×(2要素の列ベクトル)という積を考えている。
*)

(*
   Exercise 3.5
   「float * float * float * float」と「(float * float) * (float * float)」について
   前者は4次元ベクトルなどの4つの実数からなる型であるのに対して、後者は2行2列行列などの2つの実数の組2つからなる型である。
   すなわち、前者は4つ1組のtuple1つからなり、後者は2つ1組のtuple2つからなる。
*)

(*
   Exercise 3.6
   「let (x : int) = ...」におけるxも変数パターンの一種である。
   これはintにマッチして、xを...へと当てはまる値(int)に束縛させる。
*)

(* Exercise 3.7 *)
let rec pow1 (x, n) = 
  if n = 0 then 1.0 else pow1 (x, n-1) *. x;;
let rec pow2 (x, n) = 
  if n = 0 then 1.0
  else if n mod 2 = 0 then pow2 (x*.x, n/2)  else pow2 (x*.x, (n-1)/2) *. x;;

(* Exercise 3.8 *)
let powi (x, n) = 
  let rec powi' (x', n') res =
  if n' = 0 then res else powi' (x', n'-1) (res*.x')
  in powi' (x, n) 1.0;;
(*resは「本来なら残りの計算である式の結果」の情報であり、1つ上の行の1.0はあらゆる数の0乗を表している*)

(*
   Exercise 3.9
   「let cond (b, e1, e2) : int = if b then e1 else e2」「let rec fact n = cond ((n = 1), 1, n * fact (n-1))」と定義したときを考える。
   例えば fact 4 を評価する際、fact 1 において「cond ((1 = 1), 1, 1 * fact (1-1))」を計算するために、fact 0 を考えてしまっている。
   したがって、このまま fact -1, fact -2, ... と本来考えていない部分を無限に計算してしまうことになり、うまく計算できない。
*)

(*
   Exercise 3.10
   「let rec fib n = (* nth Fibonacci number *) if n = 1 || n = 2 then 1 else fib(n - 1) + fib(n - 2)」と定義したとき、fib 4 の値呼び出しによる評価ステップを考える。
   テキストに倣って記述すると、以下の通りになる。

   fib 4 -> if 4 = 1 || 4 = 2 then 1 else fib (4-1) + fib (4-2)
         -> fib (4-1) + fib (4-2)
         -> fib (4-1) + fib 2
         -> fib (4-1) + (if 2 = 1 || 2 = 2 then 1 else fib (2-1) + fib (2-2))
         -> fib (4-1) + 1
         -> fib 3 + 1
         -> (if 3 = 1 || 3 = 2 then 1 else fib (3-1) + fib (3-2)) + 1
         -> (fib (3-1) + fib (3-2)) + 1
         -> (fib (3-1) + fib 1) + 1
         -> (fib (3-1) + (if 1 = 1 || 1 = 2 then 1 else fib (1-1) + fib (1-2))) + 1
         -> (fib (3-1) + 1) + 1
         -> (fib 2 + 1) + 1
         -> ((if 2 = 1 || 2 = 2 then 1 else fib (2-1) + fib (2-2)) + 1) + 1
         -> (1 + 1) + 1
         -> 2 + 1
         -> 3
*)

(* Exercise 3.11(1) *)
let rec gcd (n, m) = (*n<=mとする*)
  if n = 1 then 1
  else if n = 0 then m else gcd (m mod n, n);;

(* Exercise 3.11(2) *)
let rec comb (n, m) = (*n>=mである*)
  if m = 0 || n = m then 1
  else comb (n-1, m) + comb (n-1, m-1);;

(* Exercise 3.11(3) *)
let fib_iter n = 
  let rec fib_iter' (m, res1, res2) = 
    if m = 1 || m = 2 then (res1, res2)
    else fib_iter' (m-1, res2, res1 + res2)
  in let (_, curr) = fib_iter' (n, 1, 1)
  in curr;;

(* Exercise 3.11(4) *)
let max_ascii str = 
  let n = String.length str
  in if n = 1 then str.[0] else
    let rec max_ascii' (n', str', res) =
      if n' + 1 = n then str'.[res]
      else if int_of_char (str'.[res]) > int_of_char (str'.[n'+1]) then max_ascii' (n'+1, str', res) else max_ascii' (n'+1, str', n'+1)
    in max_ascii' (0, str, 0);;

(* Exercise 3.12 *)
let rec arctan_one n = 
  if n < 0 then 0.0
  else arctan_one (n-1) +. (1.0 /. (float_of_int (4 * n + 1))) -. (1.0 /. (float_of_int (4 * n + 3)));;

(* Exercise 4.1 *)
let integral f a b = 
  let rec sigma (f', n') =
    if n' = 0.0 then 0.0 else f' n' +. sigma (f', n'-.1.0)
  in let trapezoid (f'', a'', b'', n'') x =
    let del = (b'' -. a'') /. n''
     in  ((f''(a''+.(x-.1.0)*.del)+.f''(a''+.x*.del))*.del)/.2.0
  in sigma ( trapezoid (f, a, b, 100000.0) , 100000.0);;
    
(* Exercise 4.2 *)
let rec pow_curry n x = 
  if n = 1 then x else x *. pow_curry (n-1) x;;
(*
  指数を第一引数とした場合は、下のようになる。
  let cube x = pow_curry 3 x;;
  指数を第二引数とした場合は下の通り。
*)
let cube x = 
  let rec pow_curry' x n =
    if n = 1 then x else x *. pow_curry' x (n-1)
  in pow_curry' x 3;;

(* 
  Exercise 4.3
  「int -> int -> int -> int」「(int -> int) -> int -> int」「(int -> int -> int) -> int」の3つについて
  1つ目はintを引数として、「intを引数として、「intを引数としてintを返す関数」を返す関数」を返す関数である。
  2つ目は「intを引数としてintを返す関数」を引数として、「intを引数としてintを返す関数」を返す関数である。
  3つ目は「intを引数として、「intを引数としてintを返す関数」を返す関数」を引数としてintを返す関数である。
  例としては、それぞれ下のように定義できる。
  let one i1 i2 i3 = i1*i2*i3
  let two (f : int -> int) = fun x -> (f (f x))
  let three (f : int -> int -> int) = (f 1) 1
*)

(* Exercise 4.4 *)
let uncurry f (x, y) = f x y;;

(* Exercise 4.5 *)
let fib_repeat n = 
  let rec repeat f n x =
    if n > 0 then repeat f (n - 1) (f x) else x
  in let f (x, y) = (y, x+y)
  in let (fibn, _) = repeat f (n - 1) (1, 1)
  in fibn;; 

(*
   Exercise 4.6
   let rec funny f n =
      if n = 0 then id
      else if n mod 2 = 0 then funny (f $ f) (n / 2)
      else funny (f $ f) (n / 2) $ f;;
   上の関数 funny は、ある引数(fがとりうる引数)に対して、関数 f を n 回適用する関数である。
   (修正)
   上の関数 funny は、ある関数 f と int n を受け取って、関数 f を n 個合成した関数を返す関数である。
*)

(* Exercise 4.7 *)
let k x _ = x;;
let s x y z = x z (y z);;
(*
  let s k k 1 -> k 1 (k 1)
              -> k 1 (何を渡しても1を返す関数)
              -> 1
  上のような計算ステップを踏むため、s k k は恒等関数となる。
*)
let second x y = k (s k k) x y;;

(*
   Exercise 4.8
   let double f x = f (f x);; と定義されているとき、
   double double f x の計算過程は以下のようになる。
   double double f x -> double f (double f x)
                     -> double f (f (f x))
                     -> f (f (f (f x)))
   したがってdouble double f x は f (f (f (f x))) として働く。
*)

(*
   Exercise 5.1
   1. 予想：a' list list 実際：
   2. 予想：第1要素は int list で、第2要素は string list であるため誤り 実際：
   3. 予想：int list list 実際：
   4. 予想：2 は int で、[3] は int list で、型が合わないため誤り 実際：
   5. 予想：a' list list 実際：
   6. 予想：bool -> bool list 実際：(bool -> bool) list （凡ミス……。）
*)

(* Exercise 5.2 *)
let hd (x::_) = x;;
let tl (_::rest) = rest;;
let null = function [] -> true | _ -> false;;
let rec sum_list l = 
  if null l then 0 else hd l + sum_list (tl l);;
let rec max_list l =
  if null (tl l) then hd l 
  else if hd l > hd (tl l) then max_list ((hd l) :: (tl (tl l))) else max_list (tl l);;
(*
  sum_list に関しては if 文 1 行で記述することができた。元の match 文 1 行よりは少し簡潔になったように思える。
  max_list に関しては 一見したときの分かりやすさが落ちたように感じるため、元の match 文の方が優れているように思える。
*)

(* Exercise 5.3(1) *)
let rec downto0 n = 
  if n = 0 then [0]
  else n :: downto0 (n-1) ;;

(* Exercise 5.3(2) *)
let roman rule n =
  let rec assoc' a = 
    if a = 0 then fun _ -> ""
    else function ((aa, b) :: rest) -> if a >= aa then b ^ (assoc' (a-aa) ((aa, b) :: rest)) else assoc' a rest
  in assoc' n rule;;

(* Exercise 5.3(3) *)
let rec concat l =
  if l = [] then []
  else if hd l = [] then concat (tl l) 
       else hd (hd l) :: concat (tl (hd l) :: (tl l))

(* Exercise 5.3(4) *)
let rec zip l1 l2 = 
  if l1 = [] || l2 = [] then [] else (hd l1, hd l2) :: zip (tl l1) (tl l2);;

(* Exercise 5.3(5) *)
let rec filter f l = 
  if l = [] then []
  else if f (hd l) then hd l :: filter f (tl l) else filter f (tl l);;

(* Exercise 5.3(6) *)
let rec insert x = function
  [] -> [x]
  | (y :: rest) as l -> if x < y then x :: l else y :: (insert x rest);;
let rec insertion_sort = function
  [] -> []
  | x :: rest -> insert x (insertion_sort rest);;

let rec belong a s =
  if s = [] then false 
  else if hd s = a then true else belong a (tl s);;
let rec intersect s1 s2 = 
  if s2 = [] then []
  else if belong (hd s2) s1 then (hd s2) :: intersect s1 (tl s2) else intersect s1 (tl s2);;
let union s1 s2 = 
  let rec union' s1 s2 =
    if s2 = [] then []
    else if belong (hd s2) s1 then union' s1 (tl s2) else (hd s2) :: union' s1 (tl s2)
  in insertion_sort (s1 @ (union' s1 s2));;
let rec diff s1 s2 = 
  if s1 = [] then []
  else if belong (hd s1) s2 then diff (tl s1) s2 else (hd s1) :: diff (tl s1) s2;;
(*
   Exercise 5.4
   let rec map f = function
   [] -> []
   | x :: rest -> f x :: map f rest;;
   上の様に map が定義されているとき、
   「map f (map g l)」は「map (fun x -> ((f x) && (g x))) l」と書き換えることができる。
   (修正)
   「map f (map g l)」は「map (fun x -> ((f $ g) x) l」と書き換えることができる。
*)

(* Exercise 5.5 *)
let rec map f = function
   [] -> []
   | x :: rest -> f x :: map f rest;;
let rec fold_right f l e =
  match l with
  [] -> e
  | x :: rest -> f x (fold_right f rest e)
  
let forall p l = 
  fold_right (fun x y -> x && y) (map p l) true;; 
let exists p l = 
  fold_right (fun x y -> x || y) (map p l) false;;

(* Exercise 5.6 *)
let rec delete n = function
  [] -> []
  | x :: rest -> if x = n then rest else x :: (delete n rest);;
let rec quicker l sorted = 
  if l = [] then sorted else quicker (delete (max_list l) l) ((max_list l) :: sorted);;
let quick l = quicker l [];;

(* Exercise 5.7 *)
let squares r =
  let rec squares' a r =
    if a*a > (r/2) then []
    else if floor (sqrt (float_of_int (r-a*a))) = sqrt (float_of_int (r-a*a))
    then ((int_of_float (sqrt (float_of_int (r-a*a)))), a) :: squares' (a+1) r
    else squares' (a+1) r
  in squares' 0 r;;

(* Exercise 5.8 *)
let rec nth n l =
  if n = 1 then hd l else nth (n - 1) (tl l);;
let rec take n l =
  if n = 0 then [] else (hd l) :: (take (n - 1) (tl l));;
let rec length = function
  [] -> 0
  | _ :: rest -> 1 + length rest;;
  
let map2 f = 
  let rec map2' (f, res) = function
    [] -> res
    | l -> map2' (f, (f (nth (length l) l) :: res)) (take ((length l) - 1) l)
  in map2' (f, []);;

(* Exercise 6.1 *)
type figure = Point | Circle of int | Rectangle of int * int | Square of int;;
type loc_fig = { x : int; y : int; fig : figure; };;
let overlap loc_fig1 loc_fig2 = 
  let lfx1 = loc_fig1.x
  in let lfx2 = loc_fig2.x
  in let lfy1 = loc_fig1.y
  in let lfy2 = loc_fig2.y
  in match (loc_fig1.fig, loc_fig2.fig) with
  (Point, Point) -> lfx1 = lfx2 && lfy1 = lfy2
  | (Point, Circle r) -> ((lfx1 - lfx2)*(lfx1 - lfx2) + (lfy1 - lfy2)*(lfy1 - lfy2)) <= r*r
  | (Point, Rectangle (lx, ly)) -> lfx2 <= lfx1 && lfx1 <= lfx2 + lx && lfy2 <= lfy1 && lfy1 <= lfy2 + ly
  | (Point, Square l) -> lfx2 <= lfx1 && lfx1 <= lfx2 + l && lfy2 <= lfy1 && lfy1 <= lfy2 + l
  | (Circle r, Point) -> ((lfx1 - lfx2)*(lfx1 - lfx2) + (lfy1 - lfy2)*(lfy1 - lfy2)) <= r*r
  | (Circle r, Circle r') -> ((lfx1 - lfx2)*(lfx1 - lfx2) + (lfy1 - lfy2)*(lfy1 - lfy2)) <= (r+r')*(r+r')
  | (Circle r, Rectangle (lx, ly)) -> (lfx2 - r <= lfx1 && lfx1 <= lfx2 + lx + r && lfy2 <= lfy1 && lfy1 <= lfy2 + ly) ||
                                      (lfx2 <= lfx1 && lfx1 <= lfx2 + lx && lfy2 - r <= lfy1 && lfy1 <= lfy2 + ly + r) ||
                                      (((lfx1 - lfx2)*(lfx1 - lfx2) + (lfy1 - lfy2)*(lfy1 - lfy2)) <= r*r) ||
                                      (((lfx1 - (lfx2 + lx))*(lfx1 - (lfx2 + lx)) + (lfy1 - lfy2)*(lfy1 - lfy2)) <= r*r) ||
                                      (((lfx1 - lfx2)*(lfx1 - lfx2) + (lfy1 - (lfy2 + ly))*(lfy1 - (lfy2 + ly))) <= r*r) ||
                                      (((lfx1 - (lfx2 + lx))*(lfx1 - (lfx2 + lx)) + (lfy1 - (lfy2 + ly))*(lfy1 - (lfy2 + ly))) <= r*r)   
  | (Circle r, Square l) -> (lfx2 - r <= lfx1 && lfx1 <= lfx2 + l + r && lfy2 <= lfy1 && lfy1 <= lfy2 + l) ||
                            (lfx2 <= lfx1 && lfx1 <= lfx2 + l && lfy2 - r <= lfy1 && lfy1 <= lfy2 + l + r) ||
                            (((lfx1 - lfx2)*(lfx1 - lfx2) + (lfy1 - lfy2)*(lfy1 - lfy2)) <= r*r) ||
                            (((lfx1 - (lfx2 + l))*(lfx1 - (lfx2 + l)) + (lfy1 - lfy2)*(lfy1 - lfy2)) <= r*r) ||
                            (((lfx1 - lfx2)*(lfx1 - lfx2) + (lfy1 - (lfy2 + l))*(lfy1 - (lfy2 + l))) <= r*r) ||
                            (((lfx1 - (lfx2 + l))*(lfx1 - (lfx2 + l)) + (lfy1 - (lfy2 + l))*(lfy1 - (lfy2 + l))) <= r*r)
  | (Rectangle (lx, ly), Point) -> lfx1 <= lfx2 && lfx2 <= lfx1 + lx && lfy1 <= lfy2 && lfy2 <= lfy1 + ly
  | (Rectangle (lx, ly), Circle r) -> (lfx1 - r <= lfx2 && lfx2 <= lfx1 + lx + r && lfy1 <= lfy2 && lfy2 <= lfy1 + ly) ||
                                      (lfx1 <= lfx2 && lfx2 <= lfx1 + lx && lfy1 - r <= lfy2 && lfy2 <= lfy1 + ly + r) ||
                                      (((lfx2 - lfx1)*(lfx2 - lfx1) + (lfy2 - lfy1)*(lfy2 - lfy1)) <= r*r) ||
                                      (((lfx2 - (lfx1 + lx))*(lfx2 - (lfx1 + lx)) + (lfy2 - lfy1)*(lfy2 - lfy1)) <= r*r) ||
                                      (((lfx2 - lfx1)*(lfx2 - lfx1) + (lfy2 - (lfy1 + ly))*(lfy2 - (lfy1 + ly))) <= r*r) ||
                                      (((lfx2 - (lfx1 + lx))*(lfx2 - (lfx1 + lx)) + (lfy2 - (lfy1 + ly))*(lfy2 - (lfy1 + ly))) <= r*r)
  | (Rectangle (lx, ly), Rectangle (lx', ly')) -> (((lfx1 <= lfx2 && lfx2 <= lfx1 + lx) || (lfx1 <= lfx2 + lx' && lfx2 + lx' <= lfx1 + lx)) || ((lfx2 <= lfx1 && lfx1 <= lfx2 + lx') || (lfx2 <= lfx1 + lx && lfx1 + lx <= lfx2 + lx'))) &&
                                                  (((lfy1 <= lfy2 && lfy2 <= lfy1 + ly) || (lfy1 <= lfy2 + ly' && lfy2 + ly' <= lfy1 + ly)) || ((lfy2 <= lfy1 && lfy1 <= lfy2 + ly') || (lfy2 <= lfy1 + ly && lfy1 + ly <= lfy2 + ly')))
  | (Rectangle (lx, ly), Square l) -> (((lfx1 <= lfx2 && lfx2 <= lfx1 + lx) || (lfx1 <= lfx2 + l && lfx2 + l <= lfx1 + lx)) || ((lfx2 <= lfx1 && lfx1 <= lfx2 + l) || (lfx2 <= lfx1 + lx && lfx1 + lx <= lfx2 + l))) &&
                                      (((lfy1 <= lfy2 && lfy2 <= lfy1 + ly) || (lfy1 <= lfy2 + l && lfy2 + l <= lfy1 + ly)) || ((lfy2 <= lfy1 && lfy1 <= lfy2 + l) || (lfy2 <= lfy1 + ly && lfy1 + ly <= lfy2 + l)))
  | (Square l, Point) -> lfx1 <= lfx2 && lfx2 <= lfx1 + l && lfy1 <= lfy2 && lfy2 <= lfy1 + l
  | (Square l, Circle r) -> (lfx1 - r <= lfx2 && lfx2 <= lfx1 + l + r && lfy1 <= lfy2 && lfy2 <= lfy1 + l) ||
                            (lfx1 <= lfx2 && lfx2 <= lfx1 + l && lfy1 - r <= lfy2 && lfy2 <= lfy1 + l + r) ||
                            (((lfx2 - lfx1)*(lfx2 - lfx1) + (lfy2 - lfy1)*(lfy2 - lfy1)) <= r*r) ||
                            (((lfx2 - (lfx1 + l))*(lfx2 - (lfx1 + l)) + (lfy2 - lfy1)*(lfy2 - lfy1)) <= r*r) ||
                            (((lfx2 - lfx1)*(lfx2 - lfx1) + (lfy2 - (lfy1 + l))*(lfy2 - (lfy1 + l))) <= r*r) ||
                            (((lfx2 - (lfx1 + l))*(lfx2 - (lfx1 + l)) + (lfy2 - (lfy1 + l))*(lfy2 - (lfy1 + l))) <= r*r)
  | (Square l, Rectangle (lx, ly)) -> (((lfx2 <= lfx1 && lfx1 <= lfx2 + lx) || (lfx2 <= lfx1 + l && lfx1 + l <= lfx2 + lx)) || ((lfx1 <= lfx2 && lfx2 <= lfx1 + l) || (lfx1 <= lfx2 + lx && lfx2 + lx <= lfx1 + l))) &&
                                      (((lfy2 <= lfy1 && lfy1 <= lfy2 + ly) || (lfy2 <= lfy1 + l && lfy1 + l <= lfy2 + ly)) || ((lfy1 <= lfy2 && lfy2 <= lfy1 + l) || (lfy1 <= lfy2 + ly && lfy2 + ly <= lfy1 + l)))          
  | (Square l, Square l') -> (((lfx2 <= lfx1 && lfx1 <= lfx2 + l') || (lfx2 <= lfx1 + l && lfx1 + l <= lfx2 + l')) || ((lfx1 <= lfx2 && lfx2 <= lfx1 + l) || (lfx1 <= lfx2 + l' && lfx2 + l' <= lfx1 + l))) &&
                             (((lfy2 <= lfy1 && lfy1 <= lfy2 + l') || (lfy2 <= lfy1 + l && lfy1 + l <= lfy2 + l')) || ((lfy1 <= lfy2 && lfy2 <= lfy1 + l) || (lfy1 <= lfy2 + l' && lfy2 + l' <= lfy1 + l)));;

(* Exercise 6.2 *)
type nat = Zero | OneMoreThan of nat;;
let rec int_of_nat n = 
  match n with
  Zero -> 0
  | OneMoreThan n' -> 1 + int_of_nat n';;
let rec add m n =
  match m with 
  Zero -> n 
  | OneMoreThan m' -> OneMoreThan (add m' n);;
let rec mul m n = 
  match m with 
  Zero -> Zero 
  | OneMoreThan m' -> add n (mul m' n);;
let rec monus m n = 
  match m with 
  Zero -> Zero 
  | OneMoreThan m' -> match n with 
                      Zero -> m
                      | OneMoreThan n' -> monus m' n';; 

(* Exercise 6.3 *)
let rec minus m n = 
  match m with 
  Zero -> if n = Zero then Some Zero else None
  | OneMoreThan m' -> match n with 
                      Zero -> Some m
                      | OneMoreThan n' -> minus m' n';;

(* Exercise 6.4 *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;
let rec comptree x n = 
  if n = 0 then Lf
  else Br (x, (comptree x (n-1)), (comptree x (n-1)));;

(* Exercise 6.5 *)
let inord t =
  let rec inord' t l =
    match t with
    Lf -> l
    | Br(x, left, right) ->  (inord' left (x :: inord' right l))
  in inord' t [];;
let postord t =
  let rec postord' t l = 
    match t with
    Lf -> l
    | Br(x, left, right) ->  (postord' left (postord' right (x :: l)))
  in postord' t [];;

(* Exercise 6.6 *)
let rec reflect t = 
  match t with
  Lf -> Lf
  | Br(x, left, right) -> Br(x, reflect right, reflect left);;
(*
p56に記載されていたように、リストを反転させる reverse 関数を定義したとき、
preorder(reflect(t)) = reverse (postorder(t))
inorder(reflect(t)) = reverse (inorder(t))
postorder(reflect(t)) = reverse (preorder(t))
が、任意の二分木 t に対して成立する。
*)

(* Exercise 6.7 *)

type arith = Const of int | Add of arith * arith | Mul of arith * arith
let rec string_of_arith _ = assert false
let rec expand _ = assert false

(*
type arith = Const of int | Add of arith * arith | Mul of arith * arith;;
let rec string_of_arith a =
  match a with
  Const n -> string_of_int n
  | Add (a1, a2) -> "(" ^ string_of_arith a1 ^ "+" ^ string_of_arith a2 ^ ")"
  | Mul (a1, a2) -> string_of_arith a1 ^ "*" ^ string_of_arith a2;;
let rec expand a = 
match a with
Const n -> Const n
| Add (a1, a2) -> Add (expand a1, expand a2)
| Mul (a1, a2) -> match (a1, a2) with
                  (Add(a1', a2'), Add(a1'', a2'')) -> Add (Add(expand (Mul(expand a1', expand a1'')), expand (Mul(expand a1', expand a2''))), Add(expand (Mul(expand a2', expand a1'')), expand (Mul(expand a2', expand a2''))))
                  | (_, Add(a1', a2')) -> Add (expand (Mul (expand a1, expand a1')), expand (Mul(expand a1, expand a2')))
                  | (Add(a1', a2'), _) -> Add (expand (Mul (expand a1', expand a2)), expand (Mul(expand a2', expand a2)))
                  | _ -> Mul (expand a1, expand a2);;
上のところまでしか出来なかった。このままだと19個中3つのテストが通らない。

ex6_7 alias test/runtest (exit 1)
(cd _build/default/test && ./ex6_7.exe)
.........E.....E..E
==============================================================================
Error: Ex.6.7:18:

File "test/ex6_7.ml", line 18, characters 23-29: Assertion failed
------------------------------------------------------------------------------
==============================================================================
Error: Ex.6.7:15:

File "test/ex6_7.ml", line 18, characters 23-29: Assertion failed
------------------------------------------------------------------------------
==============================================================================
Error: Ex.6.7:9:

File "test/ex6_7.ml", line 18, characters 23-29: Assertion failed
------------------------------------------------------------------------------
Ran: 19 tests in: 0.02 seconds.
FAILED: Cases: 19 Tried: 19 Errors: 3 Failures: 0 Skip:  0 Todo: 0 Timeouts: 0.

*)

(*
   Exersice 6.8
   1, 2, 3, 4 からなる可能な二分探索木の形を以下に列挙する。
   (1) Br(4, Br(3, Br(2, Br(1, Lf, Lf), Lf), Lf), Lf)
   (2) Br(4, Br(3, Br(1, Lf, Br(2, Lf, Lf)), Lf), Lf)
   (3) Br(4, Br(2, Br(1, Lf, Lf), Br(3, Lf, Lf)), Lf)
   (4) Br(4, Br(1, Lf, Br(3, Br(2, Lf, Lf), Lf)), Lf)
   (5) Br(4, Br(1, Lf, Br(2, Lf, Br(3, Lf, Lf))), Lf)
   (6) Br(3, Br(2, Br(1, Lf, Lf), Lf), Br(4, Lf, Lf))
   (7) Br(3, Br(1, Lf, Br(2, Lf, Lf)), Br(4, Lf, Lf))
   (8) Br(2, Br(1, Lf, Lf), Br(4, Br(3, Lf, Lf), Lf))
   (9) Br(2, Br(1, Lf, Lf), Br(3, Lf, Br(4, Lf, Lf)))
   (10)Br(1, Lf, Br(4, Br(3, Br(2, Lf, Lf), Lf), Lf))
   (11)Br(1, Lf, Br(4, Br(2, Lf, Br(3, Lf, Lf)), Lf))
   (12)Br(1, Lf, Br(3, Br(2, Lf, Lf), Br(4, Lf, Lf)))
   (13)Br(1, Lf, Br(2, Lf, Br(4, Br(3, Lf, Lf), Lf)))
   (14)Br(1, Lf, Br(2, Lf, Br(3, Lf, Br(4, Lf, Lf))))
   ここで、それぞれの木は、どの順番でaddされたかを以下に記述する。
   (1) 4 -> 3 -> 2 -> 1
   (2) 4 -> 3 -> 1 -> 2
   (3) 4 -> 2 -> 3 -> 1 / 4 -> 2 -> 1 -> 3
   (4) 4 -> 1 -> 3 -> 2
   (5) 4 -> 1 -> 2 -> 3
   (6) 3 -> 2 -> 1 -> 4 / 3 -> 2 -> 4 -> 1 / 3 -> 4 -> 2 -> 1
   (7) 3 -> 1 -> 2 -> 4 / 3 -> 1 -> 4 -> 2 / 3 -> 4 -> 1 -> 2
   (8) 2 -> 1 -> 4 -> 3 / 2 -> 4 -> 1 -> 3 / 2 -> 4 -> 3 -> 1
   (9) 2 -> 1 -> 3 -> 4 / 2 -> 3 -> 1 -> 4 / 2 -> 3 -> 4 -> 1
   (10)1 -> 4 -> 3 -> 2
   (11)1 -> 4 -> 2 -> 3
   (12)1 -> 3 -> 2 -> 4 / 1 -> 3 -> 4 -> 2
   (13)1 -> 2 -> 4 -> 3
   (14)1 -> 2 -> 3 -> 4
*)

(* Exercise 6.9 *)
type 'a seq = Cons of 'a * (unit -> 'a seq);;
let tail (Cons (_, f)) = f ();;
let rec from n = Cons (n, fun () -> from (n + 1));;
let rec sift n (Cons (x, tail)) = 
  if x mod n = 0 then sift n (tail ()) else Cons (x, fun () -> sift n (tail ()));;
let rec sieve (Cons (x, f)) = Cons (x, fun () -> sieve (sift x (f())));;
let rec nthseq n (Cons (x, f)) =
  if n = 1 then x else nthseq (n - 1) (f());;
let primes = Cons (2, fun () -> tail (sieve (from 2)));;
(*
以上の定義から、<自分の学籍番号の下４桁 + 3000 = 3587> 番目の素数は、
# nthseq 3587 primes;;
- : int = 33503
33503であることが分かった。
*)

type ('a, 'b) sum = Left of 'a | Right of 'b;;

(* Exercise 6.10(1) *)
let f1 (a, s) = 
  match s with
  Left b -> Left (a, b)
  | Right c -> Right (a, c);;

(* Exercise 6.10(2) *)
let f2 (s1, s2) = 
  match (s1, s2) with
  (Left a, Left c) -> Left (Left (a, c))
  | (Left a, Right d) -> Right (Left (a, d))
  | (Right b, Left c) -> Right (Right (b, c))
  | (Right b, Right d) -> Left (Right (b, d));;

(* Exercise 6.10(3) *)
let f3 (f1, f2) s =
  match s with
  Left a -> f1 a
  | Right c -> f2 c;;

(* Exercise 6.10(4) *)
let f4 f = ((function a -> f (Left a)), (function b -> f (Right b)));;

(* Exercise 6.10(5) *)
let f5 s = 
  match s with
  Left f -> (function a -> Left (f a))
  | Right f -> (function a -> Right (f a));;

(* Exercise 7.1 *)
type 'a ref = { mutable contents:'a };;

let ref x = { contents = x };;
let (!) r = r.contents;;
let (:=) r a = r.contents <- a;;
                                   
(* Exercise 7.2 *)
let incr r = r.contents <- (r.contents + 1);;

(*
   Exercise 7.3
   let f = ref (fun y -> y + 1)
   let funny_fact x = if x = 1 then 1 else x * (!f (x - 1));;
   以上のように定義することで、まず funny_fact x は x の2乗を計算する関数となる。
   f := funny_fact;;
   次に、代入を用いて、f の参照を funny_fact に更新すると、結果的に funny_fact は再帰的な関数のような定義になる。
*)

(* Exercise 7.4 *)
let fact_imp n = 
  let i = ref n and res = ref 1 in
    while (!i > 1) do (*"!i > 0"を"!i > 1"に修正した*)
      res := !res * !i;
      i := !i - 1;
    done;
    !res;;

(* Exercise 7.5 *)
let fact_safety n = 
  if n < 0 then raise (Invalid_argument "Invalid_argument")
    else let i = ref n and res = ref 1 in
      while (!i > 0) do
        res := !res * !i;
        i := !i - 1
      done;
      !res;;

(*
   Exercise 7.6

   let x = ref [];;
   val x : '_weak1 list ref = {contents = []}
   (2 :: !x, true :: !x);;
   Error: This expression has type int list
          but an expression was expected of type bool list
          Type int is not compatible with type bool

   授業資料では「(2 :: !x, true :: !x);;」は「- : int list * bool list = ([2], [true])」のように、
   どんな要素とも cons できると記載されていたが、実際は上のように「(2 :: !x, true :: !x);;」の時点でエラーが出るようになっている。
   参照の型は更新という動作ができるため、「(2 :: !x, true :: !x);;」の後半である「true :: !x」の評価の時点で、'_weak1 ('a のようなもの？) の型が bool に固定されて、
   前半の「2 :: !x」を評価する際に、型の違いからエラーを出力するようにしている。

   (修正)
   Errorの”This”はおそらく”true :: !x”における”!x”を指している。
   つまり、「true :: !x」より先に「2 :: !x」が評価されることで、型の違いによるエラーが発生している。
*)

(*
   Exercise 7.7
   未解決。
*)

(* Exercise 7.8 *)
let rec change = function
(_, 0) -> []
| ((c :: rest) as coins, total) ->
if c > total then change (rest, total)
else
(try
c :: change (coins, total - c)
with Failure "change" -> change (rest, total))
| _ -> raise (Failure "change");;

(*
   Exercise 7.9
   let print_int n =
    output_string stdout (string_of_int n);;
*)

(*
   Exercise 7.10
   let cp file1 file2 =
    let line = input_line (open_in file1)
    in let close1 = close_in (open_in file1)
    in let main = output_string (open_out file2) line
    in let close2 = close_out (open_out file2)
    in main; close1; close2;;
*)
