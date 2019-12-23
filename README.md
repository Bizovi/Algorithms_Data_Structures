# Algorithms and Data Structures


Work on the specialization from San Diego on Data Structures and Algorithms.



# Learning some new programming languages

## Haskell (FP101x)
Learning Haskell (and Functional programming) makes you a better computer scientist regardless of the language of choice. It is also elegant for emphasizing the algorithmic structures, take for example this `quicksort` implementation:

```haskell
qsort :: Ord a => [a] -> [a]
qsort [] = [] -- base case
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [ a | a <- xs, a <= x ]
        larger  = [ b | b <- xs, b > x ]

-- Prelude> qsort [5, 4, 1, 2, 3, 2]
```

Also, working with recursions becomes a pleasure, not the pain it was when learning C/C++. Its usefulness for understanding the foundations of `data streaming` and `data pipelines` cannot be emphasized enough.



## Julia (Optimization and ODEs)
Julia is an optimal mix between computational performance and programmer productivity which has a rapidly growing community and ecosystem of packages. It is becoming a leading force in:

* ODEs and simulating differential equations
* Optimization (`optim`)
* Operations' research
* Probabilistic programming (`Turing.jl`)
* Data processing (`JuMP`)

## Go (Web services)
Go and Haskell have little overlap. Using both makes your mind stretch and bend in different ways. Sometimes, the **sugar** of Python hides some implementation details which might be important, this is where implementing algorithms and web services in Go can bring some additional insight into the problem.


## Elm (UI and Frontend)
Instead of learning Javascript and its monstrous ecosystem, for a Data Scientist it might be easier to pick up a `statically typed`, `functional` programming language, where the compiler is your friend.

Frameworks like Dash, Shiny, Streamlit, Blogdown are amazing for quick prototyping, but once you want to go beyond the basics, you have to hack the framework. Having a tool like Elm in the arsenal helps with having that nice, fast and robutst UI for data-driven apps.