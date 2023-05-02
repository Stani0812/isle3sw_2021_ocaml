let id x = x

let uncurry2 f (x, y) = f x y
let uncurry3 f (x, y, z) = f x y z

let str_pair f1 f2 (x, y) =
  "(" ^ f1 x ^ ", " ^ f2 y ^ ")"
let str_triple f1 f2 f3 (x, y, z) =
  "(" ^ f1 x ^ ", " ^ f2 y ^ ", " ^ f3 z ^ ")"
let str_list f l =
  "[" ^ String.concat "; " (List.map f l) ^ "]"
let str_option f = function
  | None -> "None"
  | Some x -> "Some " ^ f x
let str_fun _ =
  "<fun>"

let string_of_float10 = Printf.sprintf "%.10f"

let eq_float x y =
  max (x -. y) (y -. x) < 1e-7

let gen_int () =
  Random.int (1 lsl 25) lsl 25 + Random.int (1 lsl 25)

let make_list n gen =
  Array.to_list (Array.init n gen)
