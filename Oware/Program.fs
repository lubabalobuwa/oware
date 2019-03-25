module Oware

type StartingPosition =
    | South
    | North

type Player = {
    score: int
    houses: int*int*int*int*int*int
}
type Board = {
    state: string 
    player1: Player
    player2: Player
}


let getSeeds n board = 
    let {player1 =myp1; player2 = myp2} = board
    let (h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12) = myp1.houses,myp2.houses
    match n with
        |1 -> h1
        |2 -> h2
        |3 -> h3
        |4 -> h4
        |5 -> h5
        |6 -> h6
        |7 -> h7
        |8 -> h8
        |9 -> h9
        |10 -> h10
        |11 -> h11
        |_ -> h12 //get the number of seeds from specified house

let useHouse n board = // working here
 failwith "Not implemented"

    


let start position = 
   // Takes in starting position and returns an initialized game where the person in the specified position starts the game
    let p1 = {score=0; houses= 4,4,4,4,4,4}
    let p2 = {score=0; houses= 4,4,4,4,4,4}

    let myState n = 
        match n with 
        | South -> "South's turn"
        | _ -> "North's turn"   
    let myboard = {state=myState position; player1=p1; player2=p2}
    (myboard) 


let score board = failwith "Not implemented"

let gameState board = failwith "Not implemented"


[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code