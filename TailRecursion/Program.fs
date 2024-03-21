// For more information see https://aka.ms/fsharp-console-apps
// Partial Application
module ListProcessing

// Define a simple list type
type 'x List1 =
    | Empty
    | Cons of 'x * 'x List1

// Function to process a list recursively
let rec Recursive a List2 =
    match List2 with
    | Empty -> a
    | Cons (head, tail) -> Recursive (a + head) tail

// Function to process a list using tail recursion
let ListProcessor List2 =
    let rec HelperToprocess a remainList =
        match remainList with
        | Empty -> a
        | Cons (head, tail) -> HelperToprocess (a + head) tail
    HelperToprocess 0 List2

// Example usage
// Example list [3;5;4]
let example1 = Cons(3, Cons(5, Cons(4, Empty)))

printfn "Sum of the list using tail recursion: %d" (ListProcessor example1)
