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


let useHouse n board = 
   let {player1 =myp1; player2 = myp2} = board
   let (h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12) = myp1.houses,myp2.houses
   let myBoardState = board.state
   let SouthTotal = h1+h2+h3+h4+h5+h6
   let NorthTotal = h7+h8+h9+h10+h11+h12
   let southScore = myp1.score
   let northScore = myp2.score
    
   let setSeeds myBoard currentHouse value  = 
   
       let {player1 =myp1; player2 = myp2} = myBoard
       let (h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12) = myp1.houses,myp2.houses
       let sHouses,nHouses = 
         match currentHouse with
            |1 -> ((value,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12))
            |2 -> ((h1,value,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12))
            |3 -> ((h1,h2,value,h4,h5,h6),(h7,h8,h9,h10,h11,h12))
            |4 -> ((h1,h2,h3,value,h5,h6),(h7,h8,h9,h10,h11,h12))
            |5 -> ((h1,h2,h3,h4,value,h6),(h7,h8,h9,h10,h11,h12))
            |6 -> ((h1,h2,h3,h4,h5,value),(h7,h8,h9,h10,h11,h12))
            |7 -> ((h1,h2,h3,h4,h5,h6),(value,h8,h9,h10,h11,h12))
            |8 -> ((h1,h2,h3,h4,h5,h6),(h7,value,h9,h10,h11,h12))
            |9 -> ((h1,h2,h3,h4,h5,h6),(h7,h8,value,h10,h11,h12))
            |10 -> ((h1,h2,h3,h4,h5,h6),(h7,h8,h9,value,h11,h12))
            |11 -> ((h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,value,h12))
            |_ -> ((h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,value))

       let newP1 = { myp1 with houses = sHouses }
       let newP2 = { myp2 with houses = nHouses }
       {myBoard with player1=newP1;player2=newP2}

   let checkSide state house playerTotals =
        let sTotal,_ = playerTotals
        let _,nTotal = playerTotals
        match state with
            |"South's turn" -> match house,nTotal<>0,nTotal=0&&sTotal=0 with
                                |_,_,true -> true
                                |7,true,_|8,true,_|9,true,_|10,true,_|11,true,_|12,true,_-> true
                                |_ -> false
            |_ -> match house,sTotal<>0,nTotal=0&&sTotal=0 with 
                   |_,_,true -> true
                   |1,true,_|2,true,_|3,true,_|4,true,_|5,true,_|6,true,_ -> true
                   |_ -> false

   let captureContigous myBoard captureHouse state = 
        let rec Capturing myBoard score captureHouse = 
           let {player1 =myp1; player2 = myp2} = myBoard
           let (h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12) = myp1.houses,myp2.houses

           let sTotal,nTotal = (h1+h2+h3+h4+h5+h6),(h7+h8+h9+h10+h11+h12)
           let seedNums = getSeeds captureHouse myBoard

           let newStotal,newNtotal =
            match state with
            |"South's turn" -> (sTotal),(nTotal-seedNums)
            |_ -> (sTotal-seedNums),(nTotal)

           match (checkSide state captureHouse (newStotal,newNtotal)),seedNums=2||seedNums=3 with 
            |true,true -> Capturing (setSeeds myBoard captureHouse 0 ) (score+seedNums) (captureHouse-1)
            |_ -> myBoard,score
                   
        Capturing myBoard 0 captureHouse

   let updateScore total myBoard=
        let {player1 =myp1; player2 = myp2} = myBoard
        let state = myBoard.state

        let newP1Score,newP2score,newState = 
         match state with
            |"North's turn" -> southScore,(northScore+total),"South's turn"
            |_ -> (southScore+total),northScore,"North's turn"

        let newP1 = {myp1 with score = newP1Score }
        let newP2 = {myp2 with score = newP2score}

        { myBoard with player1 = newP1;player2 = newP2; state= newState }

   let house12Check n =
       match n with
        |12 -> 1 
        |_ -> n+1 
    
   let checkFirstHouse n =  
       match n with
        |0 -> 12 
        |_ -> n 


   let CheckForEnoughSeeds myBoard houseNumber target=
            let seedNumber = getSeeds (houseNumber) myBoard 
            match (houseNumber+seedNumber)>=target with
                |true -> true 
                |_ -> false 

   let rec add value myBoard currentHouse previousHouseSeeds = 
            match value = 0 with 
             |true -> 
                     let newBoard,theScores = captureContigous myBoard (checkFirstHouse (currentHouse-1)) myBoardState 
                     updateScore theScores newBoard                                  
             |_ -> 
                   match currentHouse,currentHouse=n with
                       |12,false -> 
                               let seedNumber = (getSeeds currentHouse myBoard)+1 
                               add (value-1) (setSeeds myBoard currentHouse (seedNumber)) (1) (seedNumber)  
                       |_,false ->
                               let seedNumber = (getSeeds currentHouse myBoard)+1                           
                               add (value-1) (setSeeds myBoard currentHouse (seedNumber)) (currentHouse+1) (seedNumber) 
                       |_ -> add (value) (myBoard) (house12Check (currentHouse)) (previousHouseSeeds)

   let moveMyHouse houseNumber =
    
        match houseNumber with
           |12 -> add (getSeeds houseNumber board) (setSeeds board houseNumber 0) (1) (0)   
           |_ -> add (getSeeds houseNumber board) (setSeeds board houseNumber 0) (houseNumber) (0)  

   let TemporaryBoard houseNumber =
           match (NorthTotal=0 && board.state = "South's turn"),(SouthTotal=0 && board.state = "North's turn") with
            |true,false -> match CheckForEnoughSeeds board houseNumber 7 with 
                            |true -> moveMyHouse houseNumber 
                            |_ -> board 
            |false,true -> match CheckForEnoughSeeds board houseNumber 13 with 
                            |true -> moveMyHouse houseNumber 
                            |_ -> board 
            |_ -> moveMyHouse houseNumber 

   let theNewBoard  =
            let mySeeds = (getSeeds n board) 
            match myBoardState with 
            |"South's turn" ->  match n,mySeeds>0 with 
                            |1,true|2,true|3,true|4,true|5,true|6,true -> (TemporaryBoard n ) 
                            |_ -> { board with state = "South's turn" } 
                           
            |_ -> match n,mySeeds>0 with 
              |7,true|8,true|9,true|10,true|11,true|12,true -> (TemporaryBoard n) 
              |_ -> {board with state = "North's turn" } 
   theNewBoard 

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

let score board = 
       let {player1 =myp1; player2 = myp2} = board
       (myp1.score,myp2.score)

let gameState board =
    let {player1 =myp1; player2 = myp2} = board
    let southScore,northScore = myp1.score,myp2.score

    match southScore=24,northScore = 24 with
    |true,true -> "Game ended in a draw"
    |_ -> match southScore>=25,northScore>=25 with
            |true,false -> "South won"
            |false,true -> "North won"
            |_ -> board.state

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code