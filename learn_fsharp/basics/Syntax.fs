(*
    Getting started with the syntax and basic functionality
*)
open System

// ======== "Variables" (but not really) ==========
let x = 10
let y = 0 :: [1;2;3] @ [3..6] // concatenating lists


// ======== Functions ========
// Some really simple functions, has subtle differences from Haskell
let double (x:int): int = 
    2 * x

let quadruple (x:int): int = 
    double (double x)

let sumOfFirstSquares (n:int): int =
    // equivalent to List.sum List.map fn [x]
    List.sumBy (fun x -> x*x) [1..n] 

let netSalaryPromotion (salary:float) (taxRate:float) (raise:float): float =
    // sadly, there is no where statement as in Haskell
    let totalTax = taxRate * salary
    let promotion = salary * raise
    salary - totalTax + promotion


// ======== Recursion ========
// Again, recussion is not as elegant as in Haskell, but good enough
let rec factorial nr = 
    // but this is a lie, shouldn't accept negative integers
    match nr with
    // very important for it to be first, otherwise SOverflow
    | nr when nr < 0 -> 1  
    | 0 | 1 -> 1
    | nr -> nr * factorial (nr - 1)

// when using tail recursion should guarantee NO instruction
// is executed after it (in contrast with Haskell)
let recursiveSum list = 
    // an internal function with accumulator
    let rec _internalSum list acc = 
        match list with
            | [] -> acc
            | head::tail -> _internalSum tail (head + acc)

    // passing zero as the initial accumulator value
    _internalSum list 0  // tail position


// Finding last element with recursion:: (list:'a list): Option<'b>
// definitely some antipatterns in here
let last' list = 
    if List.isEmpty list then failwith "Empty List"
    else
        let rec _lastFn lst = 
            match lst with
            | [x] -> x
            | x::xs -> _lastFn xs
            | _ -> []  // fishy, breaks the program
        _lastFn list


// some real world use-case
let validValue: int option = Some(85)
let invalidValue: 'a option = None

let optionPatternMatch input = 
    match input with 
    | Some i -> printfn "input is an int =%d" i  // unpacks some
    | None   -> printfn "input is missing"

validValue |> optionPatternMatch
invalidValue |> optionPatternMatch


// ========= Complex Data Types =========
// tuples
let pairTuple = 1, 2
let threeTuple = "a", 2, true

// records, person1.First
type Person = {First: string; Last: string}
let person1 = {
    First = "John";
    Last  = "Doe"
}

// types can be combined recursively
type Employee = 
    | Worker of Person
    | Manager of Employee list // list of the same type
let worker = Worker person1

// union types have choices
type Temp = 
    | DegreesC of float
    | DegreesF of float
let temp = DegreesC 25.1


// ========= Printing =========
// Complex types have pretty printing built in
printfn "Printing an int: %i, float: %f, a bool: %b" 1 2.0 true
printfn "A string %s, and something generic %A" "hello" [1;2;3;4]

printfn "twoTuple=%A, \nPerson=%A, \nTemp=%A, \nEmployee=%A"
        pairTuple person1 temp worker