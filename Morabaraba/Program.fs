module Morabaraba
// A console-based game of Morabaraba: https://en.wikipedia.org/w/index.php?title=Morabaraba&oldid=701400946

// Built as a class example, so not including any "advanced" functionality
// such as using Map or Set or Seq or list expressions or active patterns
// or anything like that.

// Probably has a few bugs to iron out yet, though (of course!) fewer than
// there would likely be in the OO version.  One of the rules, in particular,
// would be an absolute nightmare to implement in OO since it involves
// testing whether half the board-state matches a past half of the board-state!
// It's fairly complex thinkery even in the FP version: see function
// excludePreviousMillRuleBreakers .  Could be made a bit easier with some
// slightly more advanced stuff, but that'd be breaking my own rules,
// wouldn't it? ;)

// License?  Hm.  Unlicense, I think: http://unlicense.org/

(*
Board:

 a7----------d7----------g7 
 | `.        |         ,' | 
 |   b6------d6------f6   | 
 |   | `.     |    ,' |   | 
 |   |   c5--d5--e5   |   | 
 |   |   |        |   |   | 
 a4--b4--c4      e4--f4--g4 
 |   |   |        |   |   | 
 |   |   c3--d3--e3   |   | 
 |   | ,'    |     `. |   | 
 |   b2------d2------f2   | 
 | ,'         |        `. | 
 a1----------d1----------g1 

*)

module Data =
    type Color =
    | Black
    | White

    type GridPosition =
    | A1 | A4 | A7
    | B2 | B4 | B6
    | C3 | C4 | C5
    | D1 | D2 | D3 | D5 | D6 | D7
    | E3 | E4 | E5
    | F2 | F4 | F6
    | G1 | G4 | G7

    type Cow =
    | Placed of Color * GridPosition
    | Flying of Color * GridPosition

    type Player = {
        Name : string
        Color : Color
        Cows : Cow list
        InHand : int
        PreviousMills : ((GridPosition * GridPosition * GridPosition) list) list
    }

    type Game = {
        Active : Player
        Passive : Player
        DrawCounter : int
    }

    type GameResult =
    | Winner of reason:string * Color
    | Draw of reason:string
    | Indeterminate

    type ListItem<'a> = {
        Description : string
        WhenChosen : unit -> 'a
    }

    type OptionScreen<'a> = {
        Prompt : string
        Choices : ListItem<InteractionResult<'a>> list
    }

    and InteractionResult<'a> =
    | InvalidInput
    | MoreChoices of OptionScreen<'a>
    | GoBack
    | Decision of 'a
    | Irrevocable of newState:'a * newRoot:OptionScreen<'a>

    type InteractionTree<'a> =
    | Root of OptionScreen<'a>
    | Node of node:InteractionTree<'a> * data:OptionScreen<'a>
    | Leaf of 'a

open Data

let swapPlayers ({Active=x;Passive=y} as g) = {g with Active=y; Passive=x}

let colorOfCow (Flying (c,_) | Placed (c,_)) = c
let positionOf (Placed (_,p) | Flying (_,p)) = p

let withColor c cows = cows |> List.filter (fun x -> colorOfCow x = c)

let allSpaces = [A1; A4; A7; B2; B4; B6; C3; C4; C5; D1; D2; D3; D5; D6; D7; E3; E4; E5; F2; F4; F6; G1; G4; G7]

let makeOptionScreen prompt choices = {Prompt=prompt; Choices=choices}

let adjacentMoves pos =
    match pos with
    | A1 -> [A4;B2;D1]
    | A4 -> [A7;B4;A1]
    | A7 -> [A4;B6;D7]
    | B2  -> [A1;B4;C3;D2]
    | B4 -> [A4;B6;C4;B2]
    | B6 -> [A7;D6;C5;B4]
    | C3 -> [C4;B2;D3]
    | C4 -> [C5;B4;D3]
    | C5 -> [B6;D5;C4]
    | D1 -> [A1;D2;G1]
    | D2 -> [D1;B2;D3;F2]
    | D3 -> [C3;E3;D2]
    | D5 -> [C5;E5;D6]
    | D6 -> [B6;D7;F6;D5]
    | D7 -> [A7;G7;D6]
    | E3 -> [D3;E4;F2]
    | E4 -> [E5;F4;E3]
    | E5 -> [D5;E4;F6]
    | F2 -> [E3;F4;D2;G1]
    | F4 -> [E4;F6;G4;F2]
    | F6 -> [D6;G7;E5;F4]
    | G1 -> [D1; F2;G4]
    | G4 -> [F4;G7;G1]
    | G7 -> [G4;F6;D7]

let occupiedSpaces g =
    (g.Active.Cows @ g.Passive.Cows) |> List.map positionOf

let placementMoves g =
    let occupied = occupiedSpaces g
    let isOccupied move = occupied |> List.exists ((=) move)
    allSpaces |> List.filter (not << isOccupied)

let millCombos =
    [   // horizontal
        A7,D7,G7
        B6,D6,F6
        C5,D5,E5
        A4,B4,C4
        E4,F4,G4
        C3,D3,E3
        B2,D2,F2
        A1,D1,G1
        // vertical
        A7,A4,A1
        B6,B4,B2
        C5,C4,C3
        D7,D6,D5
        D3,D2,D1
        E5,E4,E3
        F6,F4,F2
        G7,G4,G1
        // diagonal
        A7,B6,C5
        A1,B2,C3
        E5,F6,G7
        E3,F2,G1
    ]

let millsIn cows =
    let boardCows =
        cows |> List.map (fun c -> colorOfCow c, positionOf c)
    let cowOnGrid x = boardCows |> List.tryFind (snd >> (=) x)
    millCombos
    |> List.filter (fun (left,middle,right) ->
        match cowOnGrid left, cowOnGrid middle, cowOnGrid right with
        | _, _, None | None, _, _ | _, None, _ -> false
        | Some (c0,_), Some (c1,_), Some (c2,_) -> c0 = c1 && c1 = c2
    )

let millContains p (a,b,c) = a = p || b = p || c = p

let movesFor cows occupied =
    let isOccupied move = occupied |> List.exists ((=) move)
    let potentialMoves =
        cows
        |> List.choose (fun cow ->
            let possible =
                match cow with
                | Placed (_,p) -> Some (p, adjacentMoves p |> List.filter (not << isOccupied))
                | Flying (_,p) -> Some (p, allSpaces |> List.filter (not << isOccupied))
            match possible with
            | Some (_,[]) -> None
            | x -> x
        )
    potentialMoves

let redBull g =
    let pc =
        match g.Passive.InHand, g.Passive.Cows with
        | 0, [_;_;_] -> //FLYING!
            g.Passive.Cows |> List.map (function Placed (a,b) -> Flying (a,b) | x -> x)
        | _ -> g.Passive.Cows
    {g with Passive={g.Passive with Cows=pc}}

let shootCowAt g pos =
    let {Passive=pasv} = g
    let newPassive = {pasv with Cows=pasv.Cows |> List.filter (fun c -> positionOf c <> pos)}
    {g with Passive=newPassive; DrawCounter=0} |> redBull

let millCheck g move =
    // now, if a mill is formed with the current placement, then
    // I need to shoot a cow.
    let mills = millsIn (g.Active.Cows @ g.Passive.Cows)
    match mills |> List.exists (millContains move) with
    | true ->
        // give the option to shoot the opponent's cows!
        // now, I can only shoot cows that are NOT in a mill, UNLESS
        // ALL the opponent's cows are in a mill.  So, are all the opponent's
        // cows in a mill?
        // The more efficient way seems to be to look for cows that are not
        // in a mill, and if that list is empty, then just take all the opponent's cows
        // as targets.
        let inMill =
            g.Passive.Cows
            |> List.filter (fun c ->
                let pos = positionOf c
                mills |> List.exists (millContains pos)
            )
        let targets =
            match inMill = g.Passive.Cows with
            | true -> inMill
            | false -> g.Passive.Cows |> List.except inMill
        targets
        |> List.map (fun c ->
            let pos = positionOf c
            {Description=sprintf "Shoot cow at %A" pos; WhenChosen=fun () -> Decision <| shootCowAt g pos}
        ) |> makeOptionScreen "It's target practice time!"
        |> fun o -> Irrevocable (g, o)
    | false ->
        match g.Active.Cows, g.Passive.Cows with
        | [_;_;_], _ | _, [_;_;_] -> Decision {g with DrawCounter=g.DrawCounter+1}
        | _ -> Decision g

let placeCowAt g move =
    let {Active={InHand=handCount; Cows=cows; Color=color}} = g
    match handCount with
    | 0 -> failwith "Can't place when there are no cows!"
    | _ ->
        let newGame =
            let newCow = Placed (color, move)
            let newActive = {g.Active with InHand=handCount-1; Cows=newCow :: cows}
            {g with Active=newActive}
        millCheck newGame move

let moveCow cows from ``to`` =
    let move c p =
        match p=from with
        | true -> c, ``to``
        | false -> c, p
    cows
    |> List.map (fun c ->
        match c with
        | Flying (color,p) -> Flying (move color p)
        | Placed (color,p) -> Placed (move color p)
    )

let makeMove g from ``to`` =
    let newGame = {g with Active={g.Active with Cows = moveCow g.Active.Cows from ``to``}}
    millCheck newGame ``to``

let moveCowAt g p moves =
    moves
    |> List.map (fun move ->
        {Description=sprintf "%A" move; WhenChosen=fun () -> makeMove g p move}
    ) |> makeOptionScreen (sprintf "Move cow from %A to where?" p)
    |> MoreChoices

let excludePreviousMillRuleBreakers player moves =
    let currentMills = millsIn player.Cows // here are the current mills.
    match player.PreviousMills with
    | [] | [_] -> moves
    | _::prevMills::_ -> // these are the previous mills.
        // now, DISALLOW moves which would recreate a mill that previously existed.
        // so, using the past and the present, forbid a certain future from coming to pass!
        let newMoves =
            moves
            |> List.choose (fun (from, ``to``) ->
                match currentMills |> List.exists (millContains from) with
                | false ->
                    Some (from, ``to``) // this one's fine; it's not in a mill right now.
                | true -> // this one is in a mill right now...
                    let revised =
                        ``to`` |> List.filter (fun proposed ->
                            match prevMills |> List.filter (millContains proposed) with
                            | [] -> true // ...but making this move won't put it back into a previous mill.
                            | xs  -> // Check if this move would recreate a previously-created mill from the last turn.
                                let newMills = moveCow player.Cows from proposed |> millsIn
                                not (newMills |> List.exists (fun v -> xs |> List.contains v))
                        )
                    match revised with
                    | [] -> None // exclude entirely.  No valid move for this cow.
                    | _ -> Some (from, revised)
            )
        newMoves

let activeMoves g = occupiedSpaces g |> movesFor g.Active.Cows |> excludePreviousMillRuleBreakers g.Active
let passiveMoves g = occupiedSpaces g |> movesFor g.Passive.Cows |> excludePreviousMillRuleBreakers g.Passive

let gameResult g =
        // You win if:
        // - Your opponent cannot move.
        // - Your opponent has <= 2 cows.
        // You draw if:
        // - Either player has 3 cows, and neither shoots a cow within 10 moves
    match g.Active.InHand, g.Passive.InHand with
    | 0, 0 ->
        match activeMoves g, passiveMoves g with
        | [], [] -> // neither can move?  Draw.
            Draw "Neither player can move"
        | [], _ -> Winner (sprintf "%s can no longer move any pieces" g.Active.Name, g.Passive.Color)
        | _, [] -> Winner (sprintf "%s can no longer move any pieces" g.Passive.Name, g.Active.Color)
        | _ ->
            match g.Active.Cows, g.Passive.Cows with
            | [_;_;_], _ | _, [_;_;_] ->
                match g.DrawCounter with
                | 10 -> Draw "At least one side has 3 cows, and nobody has shot a cow in the last 10 moves"
                | _ -> Indeterminate
            | [_;_], _ | [_], _ ->
                Winner (sprintf "%s has only %d cows left" g.Active.Name (List.length g.Active.Cows), g.Passive.Color)
            | _, [_;_] | _, [_] ->
                Winner (sprintf "%s has only %d cows left" g.Passive.Name (List.length g.Passive.Cows), g.Active.Color)
            | _ -> Indeterminate
    | _ -> Indeterminate // still some cows to place.

let whiteColor = System.ConsoleColor.DarkCyan
let whiteIntenseColor = System.ConsoleColor.DarkGreen
let blackColor = System.ConsoleColor.DarkRed
let blackIntenseColor = System.ConsoleColor.DarkYellow

let printBoard g =
    let cowsAndPositions =
        (g.Active.Cows @ g.Passive.Cows)
        |> List.map (fun c -> c, positionOf c)
    let colorOf position =
        let color =
            cowsAndPositions
            |> List.tryPick (fun (c, p) ->
                match p=position with
                | true ->
                    match c with
                    | Flying (White,_) -> Some whiteIntenseColor
                    | Flying (Black,_) -> Some blackIntenseColor
                    | Placed (White,_) -> Some whiteColor
                    | Placed (Black,_) -> Some blackColor
                | false -> None
            )
        defaultArg color System.ConsoleColor.Gray
    let (~%) x =
        System.Console.ForegroundColor <- colorOf x
        printf "%A" x
    let (~%%) s =
        System.Console.ResetColor ()
        printf s
    System.Console.ForegroundColor <- whiteColor
    printf "White Cow "
    System.Console.ForegroundColor <- blackColor
    printf "Black Cow "
    System.Console.ForegroundColor <- whiteIntenseColor
    printf "White Flying Cow "
    System.Console.ForegroundColor <- blackIntenseColor
    printfn "Black Flying Cow"
    %A7; %%"----------"; %D7; %%"----------"; %G7
    %%"\n| `.        |         ,' |"
    %%"\n|   "; %B6; %%"------"; %D6; %%"------"; %F6; %%"   |"
    %%"\n|   | `.     |    ,' |   |"
    %%"\n|   |   "; %C5; %%"--"; %D5; %%"--"; %E5; %%"   |   |"
    %%"\n|   |   |        |   |   |"
    %%"\n"; %A4; %%"--"; %B4; %%"--"; %C4; %%"      "; %E4; %%"--"; %F4; %%"--"; %G4
    %%"\n|   |   |        |   |   |"
    %%"\n|   |   "; %C3; %%"--"; %D3; %%"--"; %E3; %%"   |   |"
    %%"\n|   | ,'    |     `. |   |"
    %%"\n|   "; %B2; %%"------"; %D2; %%"------"; %F2; %%"   |"
    %%"\n| ,'         |        `. |"
    %%"\n"; %A1; %%"----------"; %D1; %%"----------"; %G1
    %%"\n"

let getInput opts =
    let input = System.Console.ReadLine ()
    match input with
    | _ ->
        match input |> System.Int32.TryParse with
        | true, n ->
            match n>=1 && n<=List.length opts with
            | true ->
                let {WhenChosen=f} = opts.[n-1]
                f ()
            | _ -> InvalidInput
        | _ -> InvalidInput

let rec interact g tree =
    System.Console.Clear ()
    printBoard g
    let printOpts opts =
        System.Console.ForegroundColor <-
            match g.Active.Color with
            | White -> whiteIntenseColor | Black -> blackIntenseColor
        printfn "%s: %s" g.Active.Name opts.Prompt
        System.Console.ResetColor ()
        opts.Choices
        |> List.iteri (fun i {Description=s} -> printfn "% 2d. %s" (i+1) s)
    let waitForInput opts =
        printOpts opts
        match getInput opts.Choices with
        | InvalidInput -> interact g tree
        | MoreChoices next -> interact g (Node (tree, next))
        | GoBack ->
            match tree with
            | Root _ | Leaf _ -> failwithf "Programming error!  Received a %A when going back." tree
            | Node (parent, _) -> interact g parent
        | Decision x -> x
        | Irrevocable (newState, opts) -> interact newState (Root opts)
    match tree with
    | Root options ->
        waitForInput options
    | Node (_, options) ->
        let backChoice =
            {Description="Back to previous menu"; WhenChosen=fun () -> GoBack}
        let options = {options with Choices=options.Choices @ [backChoice]}
        waitForInput options
    | Leaf terminal ->
        terminal

let turnOptions g =
    match g.Active.InHand > 0 with
    | true -> // we're still placing cows.
        placementMoves g
        |> List.map (fun move ->
            {Description=sprintf "Place cow at %A" move; WhenChosen=fun () -> placeCowAt g move}
        ) |> makeOptionScreen (sprintf "Where would you like to place a %A cow?" g.Active.Color)
        |> Root
    | false -> // now we're moving cows.
        activeMoves g
        |> List.map (fun (p, moves) ->
            match moves with
            | [x] ->
                {Description=sprintf "Move cow from %A to %A" p x; WhenChosen=fun () -> makeMove g p x}
            | _ ->
                {Description=sprintf "Move cow at %A" p; WhenChosen=fun () -> moveCowAt g p moves}
        ) |> makeOptionScreen "Which cow would you like to move?"
        |> Root
    
let takeTurn g = turnOptions g |> interact g

let recordMills g =
    let newPrevious = // I only want to keep the last 2 around.
        let chain = (millsIn g.Active.Cows)::g.Active.PreviousMills
        match chain with
        | [] | [_] -> chain
        | a::b::_ -> [a;b]
    {g with Active={g.Active with PreviousMills=newPrevious}}

let rec startGame newGame =
    let rec playAgain () =
        printfn "Would you like to reset the board and play again? [y/n]"
        match System.Console.ReadLine () with
        | "N" | "n" -> ()
        | "Y" | "y" -> startGame newGame
        | _ -> playAgain ()
    let rec continueGame game =
        match gameResult game with
        | Winner (reason, who) ->
            printfn "%A wins!  %s." who reason
            playAgain ()
        | Draw reason ->
            printfn "The match is a draw: %s." reason
            playAgain ()
        | Indeterminate ->
            game
            |> takeTurn
            |> recordMills
            |> swapPlayers
            |> continueGame 
    continueGame newGame

let getDetails () =
    printf "What is the White player's name? "
    let p0 = {Name=System.Console.ReadLine (); Cows=[]; Color=White; InHand=12; PreviousMills=[]}
    printf "What is the Black player's name? "
    let p1 = {Name=System.Console.ReadLine (); Cows=[]; Color=Black; InHand=12; PreviousMills=[]}
    {Active=p0; Passive=p1; DrawCounter=0}

[<EntryPoint>]
let main _ = 
    getDetails () |> startGame
    0 // return an integer exit code