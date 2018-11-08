open! Base 
open! Prelude

module Example :sig 
  val the_meaning_of_list_the_universe_and_everything :int
  val subtract_one : int -> int
end = struct 
  let the_meaning_of_list_the_universe_and_everything = 42
  let subtract_one x = x - 1
end

let one_less_than_the_meaning_of_list_etc = 
  Example.subtract_one
   Example.the_meaning_of_list_the_universe_and_everything ;;
assert (one_less_than_the_meaning_of_list_etc  = 41)

module Abstract_type_example: sig
  type t
  val to_int: t->int

  val zero: t
  val one: t
  val add: t->t->t

end = struct 
  type t = int
  let to_int x = x
  let zero = 0

  let one = 1
  
  let add = (+)
end

let two = Abstract_type_example.add Abstract_type_example.one Abstract_type_example.one

let four = Abstract_type_example.to_int (Abstract_type_example.add two two)
;;


assert (four = 4)


module Fraction : sig
  type t
  val create: numerator:int -> denominator:int ->t
  val value : t-> float 

end = struct 
  type t = int * int
  let create ~numerator ~denominator = (numerator,denominator)
  let value (numerator, denominator) = 
    (Float.of_int numerator) /. (Float.of_int denominator)
  end

 let%test "Testing Fraction.value..." =
  Float.(=) 2.5 (Fraction.value (Fraction.create ~numerator:5 ~denominator:2))

let%test "Testing Fraction.value..." =
  Float.(=) 0.4 (Fraction.value (Fraction.create ~numerator:4 ~denominator:10))
