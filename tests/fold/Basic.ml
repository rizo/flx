a;;
X.a;;
X1.X2.a;;
1;;
'x';;
"hello";;

let a = 1 in
a + 1
;;

fun x -> x;;
fun x y -> x + y;;
f x;;
f x1 x2;;
f ~x1 x2;;
f ~x1 ~x2 ();;
~-1;;
a + 2;;
a + ~-2;;

match e with
| x -> x
;;

match e with
| 1 -> 0
| x -> 1
;;

match e with
| 1 -> 0
| 2 as x -> 1
;;

match e with
| 1 | 2 -> 0
| x -> 1
;;

match 1 + a with
| 1 | 2 -> 0
| x -> 1
;;

1, 2;;
a, b, 3;;
1, 2, 3, 4, 5, 6, 7, 8, 9, 10;;
1, 2, (a, b);;
[];;
true;;
false;;
`Green;;
`Rgb (255, 0, 0);;
{ x = 1 };;
{ x = 1; y = 2 };;
{ p with x = 0; z = 1 };;
{ p with x = 0 };;
[||];;
[| 1 |];;
[| 1; 2; 3 |];;
if a > 0 then e1 else e2;;
[| 1 |];;
[| 1; 2; 3 |];;

while a > 2 do
  print a
done
;;

for i = e1 to e2 do
  [| e3 |]
done
;;

for i = e1 downto e2 do
  e3
done
;;

for i = e1 to e2 do
  [| print "hello"; f () |]
done
;;

assert true;;
assert (a > 2);;
lazy 1;;
object end;;

object
  method a = 1
  method b = 2
end
;;

(* object (x) end;; *)
(* object (x : t) end;; *)

(* object (x) *)
(* method a = 1 *)
(* method b = 2 *)
(* end *)
match a with
| _ -> .
