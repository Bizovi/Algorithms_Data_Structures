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

// ======== Other tasks, as in haskell ========
let rec orderedPairs xs ys = 
    match xs, ys with
    | [], [] -> []
    | _ , [] -> []
    | [], _  -> []
    | x::xs, ys -> 
        let ysSmall = List.filter (fun z -> x <= z) ys
        List.map (fun y -> x, y) ysSmall @ orderedPairs xs ys
 
let rec flattenList = function
    | [[]] -> []
    | []   -> []
    | [xs] -> xs
    | xs::xss -> flattenList [xs] @ flattenList xss

 let rec zip xs ys = 
    match xs, ys with
    | [], _ -> []
    | _, [] -> []
    | x::xs, y::ys -> [x, y] @ zip xs ys

 let rec adjacentPairs = function
    | [] -> []
    | x::xs -> 
        let pair = 
            if List.isEmpty xs = false then [x, List.head xs] else []
        pair @ adjacentPairs xs

 printfn "%A" <| orderedPairs [1..5] [1..5]
 printfn "%A" <| flattenList [[1..4]; [2]; [3; 2], 2, 1]
 printfn "%A" <| zip [1..5] [1..5]
 printfn "%A" <| adjacentPairs [1..10]

(* fold_
    - abstract map and filter using recursive structures
    - r - from the right (empty list)
    - l - from the left

Quirk of f# - need a continuation function
*)

(* TBD: make this work, as the same mechanism as in Haskell fails
let rec foldr (list: List<'a>, func:('a -> 'b -> 'b), acc: 'b, cont:'b -> 'b) =
    match list with
    | [] -> cont acc
    | x::xs -> foldr(xs, func, acc, fun r -> cont (func x r))

let sumFold xs = 
    foldr xs (+) 0 (fun x -> x)
*)

// printfn "%A" sumFold [1..10]


(*
    Pattern matching with disjoint union types. 
    Similar to polymorphism in OOP, but with functions
*)
type Shape =  // is a type, a choice/disjoint union
    | Circle of radius:int  // isn't actually a type
    | Rectangle of height:int * width:int
    | Point of x:int * y:int
    | Polygon of pointList: (int * int) list // list of tuples of ints

let draw shape = 
    // pattern matches should be exhaustive
    // to work with shape, should handle all cases!
    match shape with 
    | Circle radius -> 
        printfn "The circle has a radius of %d" radius
    | Rectangle (height, width) ->
        printfn "The rectangle is %d high by %d wide" height width
    | Polygon points -> 
        printfn "The polygon is made of these points %A" points
    | _ -> printfn "I don't recognize this shape"

let circle = Circle(10)
let rect = Rectangle(4, 5)
let point = Point(2, 3)
let polygon = Polygon( [(1, 1); (2, 2); (3, 3)] )

[circle; rect; point; polygon] |> List.iter draw
