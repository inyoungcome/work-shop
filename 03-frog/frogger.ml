open Base
open Scaffold

module Frog = struct
  type t =
    { position : Position.t
    } [@@deriving fields]

  let create = Fields.create
end

module World = struct
  type t =
    { frog  : Frog.t
    } [@@deriving fields]

  let create = Fields.create
end

let create_frog () =
  let po = Position.create (Random.int 110) (Random.int 110) in 
  Frog.create po
;;

let create () =
  let frog = create_frog() in
    World.create frog 
;;

let tick (world : World.t) =
  let frog = create_frog() in
  {world with frog = frog}
;;

let arrow_up (frog:World.t) = 
    let p = (Field.get Frog.Fields.position (Field.get World.Fields.frog frog) )in
      let open Position in
      let chg' {x;y} =  
       let y' = y +1 in
        {x;y=y'} in
     chg' p 
;;

let arrow_down (frog:World.t) = 
    let p = (Field.get Frog.Fields.position (Field.get World.Fields.frog frog)) in
    let open Position in
    let chg' {x;y} =  
      let y' = y -1 in
       {x;y=y'} in
    chg' p
;;

let arrow_left (frog:World.t) = 
    let p = Field.get Frog.Fields.position (Field.get World.Fields.frog frog) in
    let open Position in
    let chg' {x;y} =  
      let x' = x -1 in
       {x=x';y} in
    chg' p
;;

let arrow_right (frog:World.t) = 
    let p = Field.get Frog.Fields.position (Field.get World.Fields.frog frog) in
    let open Position in
    let chg' {x;y} =  
      let x' = x + 1 in
       {x=x';y} in
    chg' p
;;

let handle_input (world : World.t) key =
  let open Key in 
  match key with
  |Arrow_up -> arrow_up world
  |Arrow_down -> arrow_down world
  |Arrow_left -> arrow_left world
  |Arrow_right -> arrow_right world
;;

let draw (world : World.t) =
  let p = Field.get Frog.Fields.position (Field.get World.Fields.frog world) in
  let open Image in 
    let image = Image.frog_down in
      [(image,p);(image,p);(image,p)]
;;

let handle_event world event =
  let open Event in 
  match event with
  |Tick -> tick world
  |Keypress key ->let p = handle_input world key in
                    {world with frog=Frog.create p}
;;

let finished world =
  false
;;