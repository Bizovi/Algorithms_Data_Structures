(*
    The goal of this project is to explore the features and syntax
    of the F# language.

    https://fsharpforfunandprofit.com/posts/conciseness-extracting-boilerplate/
    Extracting boilerplate code (this script)
*)

//  ======== Getting to the fold functions ========
// Here are three functions which do similar things (by their structure)
// Formally, homomorphisms

let product (n: int): int = 
    let initialValue = 1
    let action productSoFar x = productSoFar * x  // running total (state)
    [1..n] |> List.fold action initialValue

let sumOfOdds n = 
    let initialValue = 0
    let action sumSoFar x = if x%2 = 0 then sumSoFar else sumSoFar + x
    [1..n] |> List.fold action initialValue

let alternatingSum n = 
    let initialValue = (true, 0) // both should be passed to the next iteration
    let action (isNeg, sumSoFar) x = if isNeg then (false, sumSoFar - x)
                                              else (true, sumSoFar + x)
    [1..n] |> List.fold action initialValue |> snd  // second element of tuple

let sumOfSquares n = 
    let initialValue = 0
    let action sumSoFar x = sumSoFar + (x*x) 
    [1..n] |> List.fold action initialValue

printfn "%i" <| product 10
printfn "%i" <| sumOfOdds 10
printfn "%A" <| alternatingSum 10
printfn "%A" <| sumOfSquares 10


type NameAndSize = {Name: string; Size: int}

let maxNameAndSize list = 
    // inner function very similar to the above one
    let innerMaxNameAndSize initialValue rest = 
        let action maxSoFar x = if maxSoFar.Size < x.Size then x else maxSoFar
        rest |> List.fold action initialValue
    
    // handle empty lists
    match list with
    | [] -> None  // for an empty list
    | first::rest -> 
        let max = innerMaxNameAndSize first rest
        Some max // for a nonempty list

let list = [
    {Name="Alice"; Size=10}
    {Name="Bob"; Size=1}
    {Name="Carol"; Size=12}
    {Name="David"; Size=5}
    ]

printf "%A" <| maxNameAndSize list
printf "%A" <| maxNameAndSize list

// can wrap up the below safely for the empty case, as before
printf "%A" <| (
    list |> List.maxBy (fun item -> item.Size)
    )