(**
# First-level heading
Some more documentation using `Markdown`.
*)

let helloWorld() = printfn "Hello world!"

(** 
## Second-level heading
With some more documentation
*)

let numbers = [ 0 .. 99 ]
(*** include-value: numbers ***)

List.sum numbers
(*** include-it ***)