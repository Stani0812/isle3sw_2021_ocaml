(* Exercise 2.6 *)
val yen_of_usdollar : float -> int
val usdollar_of_yen : int -> float
val yen_of_usdollar_message : float -> string
val capitalize : char -> char

(* Exercise 3.3 *)
val geo_mean : float * float -> float

(* Exercise 3.7 *)
val pow1 : float * int -> float
val pow2 : float * int -> float

(* Exercise 3.8 *)
val powi : float * int -> float

(* Exercise 3.11(1) *)
val gcd : int * int -> int

(* Exercise 3.11(2) *)
val comb : int * int -> int

(* Exercise 3.11(3) *)
val fib_iter : int -> int

(* Exercise 3.11(4) *)
val max_ascii : string -> char

(* Exercise 3.12 *)
val arctan_one : int -> float

(* Exercise 4.1 *)
val integral : (float -> float) -> float -> float -> float

(* Exercise 4.2 *)
val pow_curry : int -> float -> float
val cube : float -> float

(* Exercise 4.4 *)
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

(* Exercise 4.5 *)
val fib_repeat : int -> int

(* Exercise 4.7 *)
val k : 'a -> 'b -> 'a
val s : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val second : 'a -> 'b -> 'b

(* Exercise 5.2 *)
val sum_list : int list -> int
val max_list : int list -> int

(* Exercise 5.3(1) *)
val downto0 : int -> int list

(* Exercise 5.3(2) *)
val roman : (int * string) list -> int -> string

(* Exercise 5.3(3) *)
val concat : 'a list list -> 'a list

(* Exercise 5.3(4) *)
val zip : 'a list -> 'b list -> ('a * 'b) list

(* Exercise 5.3(5) *)
val filter : ('a -> bool) -> 'a list -> 'a list

(* Exercise 5.3(6) *)
val belong : 'a -> 'a list -> bool
val intersect : 'a list -> 'a list -> 'a list
val union : 'a list -> 'a list -> 'a list
val diff : 'a list -> 'a list -> 'a list

(* Exercise 5.5 *)
val forall : ('a -> bool) -> 'a list -> bool
val exists : ('a -> bool) -> 'a list -> bool

(* Exercise 5.6 *)
val quicker : 'a list -> 'a list -> 'a list
val quick : 'a list -> 'a list

(* Exercise 5.7 *)
val squares : int -> (int * int) list

(* Exercise 5.8 *)
val map2 : ('a -> 'b) -> 'a list -> 'b list

(* Exercise 6.1 *)
type figure = Point | Circle of int | Rectangle of int * int | Square of int
type loc_fig = { x : int; y : int; fig : figure; }
val overlap : loc_fig -> loc_fig -> bool

(* Exercise 6.2 *)
type nat = Zero | OneMoreThan of nat
val int_of_nat : nat -> int
val add : nat -> nat -> nat
val mul : nat -> nat -> nat
val monus : nat -> nat -> nat

(* Exercise 6.3 *)
val minus : nat -> nat -> nat option

(* Exercise 6.4 *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree
val comptree : 'a -> int -> 'a tree

(* Exercise 6.5 *)
val inord : 'a tree -> 'a list
val postord : 'a tree -> 'a list

(* Exercise 6.6 *)
val reflect : 'a tree -> 'a tree

(* Exercise 6.7 *)
type arith = Const of int | Add of arith * arith | Mul of arith * arith
val expand : arith -> arith

(* Exercise 6.9 *)
type 'a seq = Cons of 'a * (unit -> 'a seq)
val primes : int seq


type ('a, 'b) sum = Left of 'a | Right of 'b

(* Exercise 6.10(1) *)
val f1 : 'a * ('b, 'c) sum -> ('a * 'b, 'a * 'c) sum

(* Exercise 6.10(2) *)
val f2 :
  ('a, 'b) sum * ('c, 'd) sum ->
  (('a * 'c, 'b * 'd) sum, ('a * 'd, 'b * 'c) sum) sum

(* Exercise 6.10(3) *)
val f3 : ('a -> 'b) * ('c -> 'b) -> ('a, 'c) sum -> 'b

(* Exercise 6.10(4) *)
val f4 : (('a, 'b) sum -> 'c) -> ('a -> 'c) * ('b -> 'c)

(* Exercise 6.10(5) *)
val f5 : ('a -> 'b, 'a -> 'c) sum -> 'a -> ('b, 'c) sum

(*Ex7.1*)
type 'a ref = { mutable contents:'a }

val ref : 'a -> 'a ref
val (!) : 'a ref -> 'a
val (:=) : 'a ref -> 'a -> unit
                                   
(*Ex7.2*)
val incr : int ref -> unit

(* Exercise 7.4 *)
val fact_imp : int -> int

(* Exercise 7.5 *)
val fact_safety : int -> int

(* Exercise 7.8 *)
val change : int list * int -> int list
