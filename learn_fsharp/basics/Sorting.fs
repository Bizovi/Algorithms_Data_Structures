(*
    Implementing more recursisve algorithms, including
    QuickSort, Cartesian Product, Ordered Pairs
    Implementing some functions from the standard librarys
*)

let rec qsort list =
    match list with
    | [] -> []  // base case
    | x::xs -> 
        let smaller = 
            xs // identation is very important here
            |> List.filter (fun e -> e < x)
            |> qsort
        let larger = 
            xs
            |> List.filter (fun e -> e >= x)
            |> qsort
        List.concat [smaller; [x]; larger]

// a more compact, slightly less readable implementation
let rec qsort2 = function
    | [] -> []
    | first::rest ->
        let smaller, larger = List.partition ((>=) first) rest
        List.concat [qsort2 smaller; [first]; qsort2 larger]

printfn "%A" (qsort [1;5;23;18;9;3])
printfn "%A" (qsort2 [1;5;23;18;9;3;3])

// reverse a function recursively
let rec reverse = function
    | [] -> []
    | x::xs -> reverse xs @ [x]

let rec take n list = 
    match n, list with
    | 0,  _ -> []
    | _, [] -> []
    | k, x::xs -> 
        let k' = k - 1
        [x] @ take k' xs

let rec map fn xs = 
    match fn, xs with
    | _, [] -> []
    | fn, x::xs -> [fn x] @ map fn xs

let rec filter' fn xs = 
    match fn, xs with
    | _ , [] -> []
    | fn, x::xs -> 
        if fn x = true then [x] @ filter' fn xs
        else if fn x = false then [] @ filter' fn xs
        else []

printfn "%A" <| reverse [1..10]
printfn "%A" <| take 3 [1..10]
printfn "%A" <| map (fun x -> x*x) [1..10]
printfn "%A" <| filter' (fun x -> x%2 = 0) [1..10]

let rec recursiveCartesian xs ys = 
    match xs, ys with
    | [], [] -> []
    | [], _  -> []
    | _ , [] -> []
    | x::xs, ys -> List.map (fun y -> x, y) ys @
                   recursiveCartesian xs ys

printfn "%A" <| recursiveCartesian [1..5] [1..5]

// ======== List comprehensions ========
// think of generators in Python, yield! [a..b] syntax
let squaredEvens n = 
    [ for a in [1..n] do 
        if a%2 = 0 then yield (a*a)
    ] 

let cartesian xs ys = 
    [ for x in xs do
      for y in ys -> x,y
    ]

printfn "%A" <| squaredEvens 10
printfn "%A" <| cartesian [1..5] [1..5]