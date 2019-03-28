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


 // Colloects , Sows and Captures
let useHouse n board = 
   let {player1 =myp1; player2 = myp2} = board
   let (h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12) = myp1.houses,myp2.houses
   let myBoardState = board.state
   let SouthTotal = h1+h2+h3+h4+h5+h6
   let NorthTotal = h7+h8+h9+h10+h11+h12
   let southScore = myp1.score
   let northScore = myp2.score
    
   let setSeeds myBoard currentHouse value  = 
   // Given the house number and value. It sets the number of seeds in the given house to the given value
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
       {myBoard with player1=newP1;player2=newP2} // Return updated board with given house set to specified value

   let checkSide state house playerTotals =
   // Check if player is capturing at opponent side
   // And checks if you can capture opponent seeds without removing all seeds from opponents board
        let sTotal,_ = playerTotals
        let _,nTotal = playerTotals
        match state with
            |"South's turn" -> match house,nTotal<>0,nTotal=0&&sTotal=0 with
                                |_,_,true -> true // Both players have no seeds and thus game is a draw
                                |7,true,_|8,true,_|9,true,_|10,true,_|11,true,_|12,true,_-> true // Is at opponent side. Can capture
                                |_ -> false // cannot capture
            |_ -> match house,sTotal<>0,nTotal=0&&sTotal=0 with 
                   |_,_,true -> true // Both players have no seeds and thus game is a draw
                   |1,true,_|2,true,_|3,true,_|4,true,_|5,true,_|6,true,_ -> true // Is at opponent side. Can capture
                   |_ -> false // cannot capture

   let captureContigous myBoard captureHouse state = 
   // Captures seeds from opponent 

        let rec Capturing myBoard score captureHouse = 
           let {player1 =myp1; player2 = myp2} = myBoard
           let (h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12) = myp1.houses,myp2.houses

           let sTotal,nTotal = (h1+h2+h3+h4+h5+h6),(h7+h8+h9+h10+h11+h12)
           let seedNums = getSeeds captureHouse myBoard // Get the number of seeds to capture/take

           let newStotal,newNtotal =
            match state with
            |"South's turn" -> (sTotal),(nTotal-seedNums) // Take seeds from North side
            |_ -> (sTotal-seedNums),(nTotal) // Take seed from South side

           match (checkSide state captureHouse (newStotal,newNtotal)),seedNums=2||seedNums=3 with  // Check if you can capture
            |true,true -> Capturing (setSeeds myBoard captureHouse 0 ) (score+seedNums) (captureHouse-1) // Can capture , check if you can capture at contigious Houses
            |_ -> myBoard,score // Done capturing . Return the new updated board and score
                   
        Capturing myBoard 0 captureHouse // lets Capture!!

   let updateScore total myBoard=
   // Updates the game board
        let {player1 =myp1; player2 = myp2} = myBoard
        let state = myBoard.state

        let newP1Score,newP2score,newState = 
         match state with
            |"North's turn" -> southScore,(northScore+total),"South's turn" // return new North's score and change state to South's turn
            |_ -> (southScore+total),northScore,"North's turn" // return new South's score and change state to North's turn

        let newP1 = {myp1 with score = newP1Score }  // update player 1 scores
        let newP2 = {myp2 with score = newP2score}   // update player 2 scores

        { myBoard with player1 = newP1;player2 = newP2; state= newState } // update the game board with players and state. And then returns the Board

   let house12Check n =
   // check if at  house number 12
       match n with
        |12 -> 1  // Go to house 1
        |_ -> n+1  // Go to the next house
    
   let checkFirstHouse n =  
   // check if (house number)-1 = 0
       match n with 
        |0 -> 12  // return house 12
        |_ -> n // return current house number
         

   let CheckForEnoughSeeds myBoard houseNumber target=
   // Check if given house collected seeds will be enough for sowing such that the opponents house/houses get seeds
            let seedNumber = getSeeds (houseNumber) myBoard 
            match (houseNumber+seedNumber)>=target with
                |true -> true // Collected seeds are enough
                |_ -> false  // Collected seeds are not enough


   (*
      add value myBoard currentHouse previousHouseSeeds
       value -> number of seeds to distribute during sowing
       currentHouse -> house to drop seed in
       previousHouseSeeds -> number of seeds in the previous house
     *)
   let rec add value myBoard currentHouse previousHouseSeeds = 
            // Do it now!!
            match value = 0 with  // check if done sowing
             |true ->  // done sowing
                     let newBoard,theScores = captureContigous myBoard (checkFirstHouse (currentHouse-1)) myBoardState  // returns newBoard and score after capturing all capturable seeds and binds them to newBoard and theScores
                     updateScore theScores newBoard // Returns new board with updated score and player houses                               
             |_ -> // not done sowing
                   match currentHouse,currentHouse=n with
                       |12,false -> 
                               let seedNumber = (getSeeds currentHouse myBoard)+1 // add one to the current house
                               add (value-1) (setSeeds myBoard currentHouse (seedNumber)) (1) (seedNumber)  // continue sowing from house 1
                       |_,false ->
                               let seedNumber = (getSeeds currentHouse myBoard)+1    // add one to the current house                        
                               add (value-1) (setSeeds myBoard currentHouse (seedNumber)) (currentHouse+1) (seedNumber)  // continue sowing
                       |_ -> add (value) (myBoard) (house12Check (currentHouse)) (previousHouseSeeds) // HAVE REACHED HOUSE WE COLLECTED SEEDS FROM. SKIP AND CONTINUE SOWING FROM NEXT HOUSE 

   let moveMyHouse houseNumber =
    // collects from specifiend and sows and captures 
        match houseNumber with
           |12 -> add (getSeeds houseNumber board) (setSeeds board houseNumber 0) (1) (0) // if its 12 start sowing from house 1 
           |_ -> add (getSeeds houseNumber board) (setSeeds board houseNumber 0) (houseNumber) (0)  // if its any other house start sowing from the next house

   let TemporaryBoard houseNumber =
           match (NorthTotal=0 && board.state = "South's turn"),(SouthTotal=0 && board.state = "North's turn") with // Checks if South or North has any seeds on their sides
            |true,false -> match CheckForEnoughSeeds board houseNumber 7 with  // South does not have any seeds, CheckForEnoughSeeds
                            |true -> moveMyHouse houseNumber  // Collected seeds enough, So continue with collecting,sowing and capturing
                            |_ -> board  // Collected seeds not enough, Cannot sow using current house, Return the board state
            |false,true -> match CheckForEnoughSeeds board houseNumber 13 with  // North does not have any seeds, CheckForEnoughSeeds
                            |true -> moveMyHouse houseNumber  // Collected seeds enough, So continue with collecting,sowing and capturing
                            |_ -> board // Collected seeds not enough, Cannot sow using current house, Return the board state
            |_ -> moveMyHouse houseNumber  // North and South have plenty of seeds on their respective sides, So continue with collecting,sowing and capturing

   let theNewBoard  =
   //get the seeds from house
            let mySeeds = (getSeeds n board) 
            match myBoardState with  //check who's turn it is
            |"South's turn" ->  match n,mySeeds>0 with  // Only collect,sow and capture if house is not empty and if you are sowing on South side
                            |1,true|2,true|3,true|4,true|5,true|6,true -> (TemporaryBoard n )  // collect,sow and capture
                            |_ -> { board with state = "South's turn" } // Do not do anything. South should choose another house to collect from
                           
            |_ -> match n,mySeeds>0 with // Only collect,sow and capture if house is not empty and if you are sowing on North side
              |7,true|8,true|9,true|10,true|11,true|12,true -> (TemporaryBoard n) // collect,sow and capture
              |_ -> {board with state = "North's turn" } // Do not do anything. North should choose another house to collect from
   theNewBoard // return the new updated game board

let start position = 
   // Takes in starting position and returns an initialized game where the person in the specified position starts the game
    let p1 = {score=0; houses= 4,4,4,4,4,4}
    let p2 = {score=0; houses= 4,4,4,4,4,4}

    let myState n = 
        match n with 
        | South -> "South's turn"
        | _ -> "North's turn"   
    let myboard = {state=myState position; player1=p1; player2=p2}
    (myboard) // return the updated game board

let score board = 
       let {player1 =myp1; player2 = myp2} = board
       (myp1.score,myp2.score) //returns the score of both players

let gameState board =
// Checks the status of the game
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