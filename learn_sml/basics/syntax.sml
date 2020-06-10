(* This is an introduction to the fundamentals of Standard ML 
 * - Variables, Types and data structures
 * - Functions, Lists, Tuples, Tail Recursion
 * - Get a sense of the ~syntax~ and ~semantics~ of the language
 * - IMMUTABLE data structures!
 *)


(* PT1:
 * Static Types, Bindings and Expressions - fiddling at the REPL
 * Note that the types can't be changed at runtime
 * The ~static environment~ checks the types
 * Evaluate statements in the current ~dynamic environment~ (lookup value)
 *)

(* Booleans and boolean expressions - better style!
 * x andalso y => if x false return false, else result of y (&&)
 * x orelse y => if x is false eval y, else return true (||)
 * not x => negation (! reserved for mutation behavior)
 *)

(* CAN'T USE = on reals to check for equality !! *)

val greeting : string = "Hello!"  (* A value, produces itself *)
val piApprox : real = 22.0 / 7.0  (* Not all expressions are values *)
val stringLength : int = size "Hello World!"


(* Working with tuples and lists
 * Tuple heterogenous/nested data structure with a fixed number of elements
 * Tuples are records (maps) behind the scenes {1: a, 2: b}
 * Think of :name accessor for records in Clojure === #1 in SML
 *)

fun swap(pair : int*bool) = 
  (#2 pair, #1 pair)

fun div_mod(x : int, y : int) : int * int = 
  (x div y, x mod y)

fun sort_pair(pr : int * int) = 
  if (# 1 pr) < (#2 pr) then pr 
  else (#2 pr, #1 pr)


(* Lists:
 * Notice that a list has a variable size, but same type of elements 
 * Can use null xs to know if it's empty
 *)

val users = 
  [("Tom", 42, "Engineer", ("Bachelors", "UK", 4))
  ,("Marie", 25, "Mathematician", ("Masters", "France", 5))
  ,("Anne", 35, "Economist", ("PhD", "US", 10))
  ]

val tom = hd users    (* syntactic sugar for x::xs -> x *)
val ladies = tl users (* x::xs -> xs *)


(* Functions exploration:
 * Type Inference - consistency of declared result and actuals
 * Currying, Recursion, Tail Recursion, Pattern Matching
 *)

fun abs z = if z < 0 then 0 - z else z

fun customMult(x, y) = x * y
fun double x = customMult(x, 2)
fun triple x = customMult(3, x)


fun sumList (xs: int list) =
  if null xs then 0
  else hd xs + sumList(tl xs)


fun reverseList [] = []
  | reverseList (x::xs) = (reverseList xs) @ [x]

fun countdown (n : int) =
  case n of
      0 => [0]
    | n => if n > 0 then [n] @ countdown(n-1) else [n] @ countdown(n + 1)

(* Implementing the concatenation @ operator, using hd and tl *)
fun append ([], ys) = ys
  | append (xs, []) = xs
  | append (xs, ys) = (hd xs) :: append((tl xs), ys)


(* This exampe highlights a few patterns and techniques:
 * Pattern matching, scoping (let), proto-foldr / accumulators
 * Raising errors / Warnings
 * Very easy for an integer overflow though
 *)
fun factorial n = 
  let
    fun fac 0 acc = acc
      | fac n acc = fac (n - 1) (n * acc)
  in
    if (n < 0) then raise Fail "negative argument not allowed"
    else fac n 1
  end

(* Important that the type is specified, as ML can't deal with arbitrary tuples *)
fun sumPairList(xs : (int * int) list) = 
  if null xs then 0
  else #1 (hd xs) + #2 (hd xs) + sumPairList(tl xs)


(* Tackle the unnecessary / nasty case of negative powers 
 * Needs to be a more elegant way to handle output of multiple types
*)
fun pow(x: int, y: int) : int = 
  if y = 0 then 1
  else x * pow(x, y-1)

fun cube x : int = pow(x, 3)

(* 'a list and f : 'b -> 'c *)
fun map([] : 'a list, _) = []
  | map(x::xs, f) = [f(x)] @ map(xs, f)



(* An implementation of the quicksort algorithm
 * Note that we haven't introduced anonymous functions / list commprehensions yet
 * So, have to implement everything from scratch (even the prefix operation)
 * Needs a filtering function as a helper
 *)

fun filter([], _) = []
  | filter(x::xs, f) = if f(x) = true then [x] @ filter(xs, f) else filter(xs, f)

fun gt(k, x) = if k > x then true else false
fun le(k, x) = if k <= x then true else false

fun quicksort([], _) = []
  | quicksort (x::xs, reverse) = 
    let
      val smaller = case reverse of
          true  => filter(xs, fn (y) => x <= y)
        | false => filter(xs, fn (y) => x > y)

      val larger = case reverse of
          true  => filter(xs, fn (y) => x > y)
        | false => filter(xs, fn (y) => x <= y)
    in
      quicksort(smaller, reverse) @ [x] @ quicksort(larger, reverse)
    end


(*  TODO(Mihai) - Implement foldr
 * more advanced FP concepts
 *)


 (* More on nested functions and scoping
  * Note that sometimes we can get rid of the unnecessary parameters
  * Try to avoid doing computations repeatedly - tremendously important
  * Rule of thumb - don't apply recursion in multiple places
  *)

fun xrange (x : int) = 
  (* Make sure it is used only within this expression *)
  let 
    fun range (from : int) = 
    if from = x then x::[] else from::range(from+1)
  in
    range(1)
  end
  

(* An excellent example when to avoid repeated computations:
 * Saving recursive results in local bindings is essential
 *)

(* Options: how to treat an empty list?
 * We can raise an exception or return something useful
 * raise Fail "Can't apply a max on an empty list"
 * SOME: isSome x; valOf x (exception if given none)
 *)

fun max ys =
  case ys of
    [] => raise Fail "Can't apply a max on an empty list"
  | [x] => x
  | x::xs => 
      let val tail_max = max xs
      in
        if x > tail_max then x
        else tail_max
      end


(* fn : int list -> int option 
 * A clear separation between the NONE case and SOME case
 *)
fun maxOption [] = NONE
  | maxOption (xs : int list) = 
      let 
        fun maxNonempty (xs : int list) =  (* The important function *)
          case xs of
            []  => raise Fail "Something went terribly wrong"
          | [y] => y
          | y::ys => 
              let val tl_ans = maxNonempty(ys)
              in 
                if y > tl_ans then y else tl_ans 
              end
      in SOME (maxNonempty xs)
      end


