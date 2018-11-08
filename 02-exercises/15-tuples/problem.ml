open! Base 

type in_char_string = int * char * string

let example = (5,'A',"Hello")

let (i,c,s) = example

let () = 
  assert (i=5);
  assert (Char.equal c  'A');
  assert (String.(=) s "Hello")

type coordinate = int * int

let add coord1 coord2 = 
 let (x1,y1) = coord1 in
 let (x2,y2) = coord2 in
  (x1+x2 ,y1+y2)
type name  = string * string

type initials = char * char

type 'a pair  = 'a * 'a

let int_pair: int pair = (7,7)
let string_pair: string pair = ("foo","bar")
let nested_char_pair : (char pair) pair = (('a','b'),('c','d'))

let first pair = 
  let (x,y) = pair in
  x

let second pair = 
  let (x,y) = pair in
  y

  let%test "Testing add..." =
  [%compare.equal: int*int] (4,7) (add (5,3) (-1,4))

  let%test "Testing first..." =
  String.(=) "foo" (first ("foo","bar"))

  let%test "Testing second..." =
  Char.(=) 'b' (second ('a','b'))
