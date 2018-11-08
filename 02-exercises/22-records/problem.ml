open! Base 

type person = {
  age: int;
  first_name: string;
  last_name:string;
  number_of_cars:int
}[@@deriving compare]

let an_example:person =
  {first_name = "Conten_eye"
  ;last_name="Joe"
  ;age=22
  ;number_of_cars = 0}

let age:int = an_example.age
let() = assert(age=22)

let print_inof {first_name;last_name;age;number_of_cars} = 
  Stdio.print_endline first_name;
  Stdio.print_endline last_name;
  Stdio.printf "Age: %d #of cars : %d\n" age,number_of_cars ;;

let print_inof {first_name;last_name;age=_;number_of_cars=_} = 
  Stdio.print_endline first_name;
  Stdio.print_endline last_name


let add_one_to_age person =
  {person with age = person.age + 1}

let () = assert (23 = (add_one_to_age an_example).age)

let modify_person (person:person) = 
  let name = person.first_name in
  match name with
  |"Jan" -> {person with age = 30}
  |_ ->{person with number_of_cars = person.number_of_cars + 6}

module For_testing = struct
  let test_ex1 :person = {
    first_name = "Jan"
    ;last_name = "Saffer"
    ;age = 55
    ;number_of_cars =0;
  };;
  let test_ex1' : person = {test_ex1 with age = 30};;

  let test_ex2 : person = {
    first_name = "Hugo";
    last_name = "Heuzard";
    age = 4;
    number_of_cars = 55;
  };;

  let test_ex2' : person = { test_ex2 with number_of_cars = 61};;

  let%test "Testing modify_person..." =
    [%compare.equal: person] test_ex1' (modify_person test_ex1)
  ;;

  let%test "Testing modify_person..." =
    [%compare.equal: person] test_ex2' (modify_person test_ex2)
  ;;
end