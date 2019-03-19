module Oware

type StartingPosition =
    | South
    | North

type State = 
    | SouthTurn of string
    | NorthTurn of string 
    | GameEnd of string 
    | SouthWin of string
    | NorthWin of string 
    | Draw of string 

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
    let (h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12) = board
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
        |_ -> h12

let useHouse n board = // working here
    let (h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12) = board
    match n with
    |1 -> (0,5,5,5,5,4),(4,4,4,4,4,4)
    |2 -> (4,0,5,5,5,5),(4,4,4,4,4,4)
    |3 ->  (4,4,0,5,5,5),(5,4,4,4,4,4)
    |4 ->  (4,4,4,0,5,5),(5,5,4,4,4,4)
    |5 -> (4,4,4,4,0,5),(5,5,5,4,4,4)
    |6 -> (4,4,4,4,4,0),(5,5,5,5,4,4)
    |7 -> (4,4,4,4,4,4),(0,5,5,5,5,4)
    |8 -> (4,4,4,4,4,4),(4,0,5,5,5,5)
    |9 -> (5,4,4,4,4,4),(4,4,0,5,5,5)
    |10 -> (5,5,4,4,4,4),(4,4,4,0,5,5)
    |11 -> (5,5,5,4,4,4),(4,4,4,4,0,5)
    |_ -> (5,5,5,5,4,4),(4,4,4,4,4,0)



let start position = 

    let p1 = {score=0; houses= 4,4,4,4,4,4}
    let p2 = {score=0; houses= 4,4,4,4,4,4}

    let myState n = 
        match n with 
        | South -> "South's turn"
        | _ -> "North's turn"   
    let board = {state=myState position; player1=p1; player2=p2}
    (p1.houses,p2.houses)


let score board = failwith "Not implemented"

let gameState board = failwith "Not implemented"


[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
