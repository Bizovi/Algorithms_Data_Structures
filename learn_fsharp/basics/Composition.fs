// a lot of things done by composition 
// which have different patterns in OOP

let add2 x = x + 2
let mult3 x = x * 3
let square x = x * x

// layman order, from left to right with >>
let add2ThenMult3 = add2 >> mult3  
let mult3ThenSquare = mult3 >> square

[1..10] |> List.map add2ThenMult3 |> printfn "%A"
[1..10] |> List.map mult3ThenSquare |> printfn "%A"


// a more relevant example
// some logging behavior
let logMsg msg x = printf "%s%i" msg x; x    // without linefeed, x returned
let logMsgN msg x = printfn "%s%i" msg x; x  // with linefeed, x returned

// they all implicitly have x as the argument (rails picture)
let mult3ThenSquareLogged =  
    logMsg "before="  // msg applied -> logMsg x now
    >> mult3
    >> logMsg " after mult3="
    >> square
    >> logMsgN " result="

[1..10] |> List.map mult3ThenSquareLogged 


// compose a list of functions in a single operation
let listOfFunctions = [
    mult3; square; add2; logMsgN "result=";
]

let allFunctions = List.reduce (>>) listOfFunctions
[1..10] |> List.map allFunctions



//  ======== Domain-specific languages ========
// can have own layer, parser, etc or 
// design set of verbs, nouns that encapsulate behavior

// vocabulary
type DateScale = Hour | Hours | Day | Days | Week | Weeks
type DateDirection = Ago | Hence

// define function that matches on the vocabulary
let getDate interval scale direction = 
    let absHours = match scale with
        | Hour | Hours -> 1 * interval
        | Day | Days -> 24 * interval
        | Week | Weeks -> 24 * 7 * interval

    let signedHours = match direction with
        | Ago -> -1 * absHours
        | Hence -> absHours

    System.DateTime.Now.AddHours(float signedHours)

// test some examples
let example1 = getDate 5 Days Ago
let example2 = getDate 1 Hour Hence



//  ======== Domain-specific languages (shapes) ========
// Fluent shapes (maybe for drawing interface)
// all function should return an object that can be used by next one
type FluentShape = {
    label: string;
    color: string;
    onClick: FluentShape -> FluentShape; // function type
}

// lowest level of abstraction
let defaultShape = 
    {label=""; color=""; onClick=fun shape -> shape}

let click shape = 
    shape.onClick shape

let display shape = 
    printfn "My label=%s and my color=%s" shape.label shape.color
    shape // return same shape

let setLabel label shape = 
    {shape with FluentShape.label = label}

let setColor color shape = 
    {shape with FluentShape.color = color}

// add a click action 
let appendClickAction action shape = 
    {shape with FluentShape.onClick = shape.onClick >> action}

// composing some of the above
let setRedBox = setColor "red" >> setLabel "box"
let setBlueBox = setRedBox >> setColor "blue"
let changeColorOnClick color = appendClickAction (setColor color) // special case

// setup some values for testing
let redBox = defaultShape |> setRedBox
let blueBox = defaultShape |> setBlueBox

// shape that changes color when clicked
redBox
    |> display
    |> changeColorOnClick "green"
    |> click
    |> display // new version after click

// changes shape and label when clicked
blueBox 
    |> display
    |> appendClickAction (setLabel "box2" >> setColor "green")
    |> click
    |> display

// an even better example
let rainbow = 
    ["red"; "orange"; "yellow"; "green"; "blue"; "indigo"; "violet"]

let showRainbow = 
    let setColorAndDisplay color = setColor color >> display
    rainbow
    |> List.map setColorAndDisplay
    |> List.reduce (>>)

// test
defaultShape |> showRainbow