module Morabaraba.Data

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
| SaveGame of string
| LoadGame of string

type InteractionTree<'a> =
| Root of OptionScreen<'a>
| Node of node:InteractionTree<'a> * data:OptionScreen<'a>
| Leaf of 'a