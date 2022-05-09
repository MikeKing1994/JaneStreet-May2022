module Operators = 
    let (/) x y = 
        (x |> double) / (y |> double)

open Operators

type Input = 
    {
        d:double
        r: int
        m: int
        n: int
    }

let mutable known : Map<(double*int*int*int), double> = Map.empty

//let rec p (known: Map<(double*int*int*int), double>) (d:double) (r: int) (m: int) (n: int): double = 
let rec p (d:double) (r: int) (m: int) (n: int): double = 
    //printfn "d: %A, r: %d, m:%d, n:%d" d r m n

    match known.TryFind (d, r, m, n) with
    | Some prev ->
        //printfn "found previous result: %A = %A" (d, r, m, n) prev
        prev 
    | None -> 
        if n = 0 then 
            //printfn "n was 0"
            let key = (d, r, m, n)            
            known <- known.Add (key, (double 1))
            double 1
        else if n > 0 && r = 0 then
            //printfn "r was 0"
            let key = (d, r, m, n)
            known <- known.Add (key, (double 1))
            double 0 
        else 
            ((double 1-d) + (d*(double)m)/(m+n))*(p d (r-1) m n) + d*((n/(m+n))*(p d (r-1) (m+1) (n-1)))
            
            //(double 1-d)*(p d (r-1) m n) + d*((m/(m+n))*(p d (r-1) m n) + (n/(m+n))*(p d (r-1) (m+1) (n-1)))
            //(m/(m+n))*(p (r-1) m n) + (n/(m+n))*(p (r-1) (m+1) (n-1))

//let list = 
//    [ 
//        double 0.1
//    ]

let baseD = double 0.5

for i in [0..10] do
    printfn "d: %A, r: %d, m:%d, n:%d" baseD i 0 8

    let result = p baseD i 0 8
    printfn "%A" result

    let key = (baseD, i, 0, 8)
    known <- known.Add (key, result)

for i in [0..10] do
    let key = (baseD, 23, i, 8)
    printfn "%A" key

    let result = p baseD 23 i 8
    printfn "%A" result

    known <- known.Add (key, result)

for i in [0..5] do
    let key = (baseD, 23, 0, i)
    printfn "%A" key

    let result = p baseD 23 0 i
    printfn "%A" result

    known <- known.Add (key, result)