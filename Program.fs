module Operators = 
    let (/) x y = 
        (x |> double) / (y |> double)

open Operators

module Exponent = 
    type Exponent = 
        {
            Coeff: double
            Power: int
        }

    let (^^) (x: double) (y: Exponent) = 
        {
            Coeff = x * y.Coeff
            Power = y.Power
        }

    let (^^^) (x: Exponent) (y: Exponent) = 
        {
            Coeff = x.Coeff * y.Coeff
            Power = x.Power + y.Power 
        }

    let multiplyOut (x: Exponent list) (y: Exponent list): Exponent list = 
        let list = []
        for a in x do 
            for b in 

open Exponent

let mutable known : Map<(double*int*int*int), double> = Map.empty

let rec p (d:double) (r: int) (m: int) (n: int): double = 

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


let rec p2 (d:double) (r: int) (m: int) (n: int): Exponent list = 

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

let list = 
    [ 
        double 0.1
        double 0.2
        double 0.3
        double 0.4
        double 0.5
        double 0.6
        double 0.7
        double 0.8
        double 0.9
    ]

let baseD = double 0.5

for baseD in list do 
    let result = p baseD 23 0 8
    printfn "%A: %A" baseD result
    

//for i in [0..10] do
//    printfn "d: %A, r: %d, m:%d, n:%d" baseD i 0 8
//
//    let result = p baseD i 0 8
//    printfn "%A" result
//
//    let key = (baseD, i, 0, 8)
//    known <- known.Add (key, result)
//
//for i in [0..10] do
//    let key = (baseD, 23, i, 8)
//    printfn "%A" key
//
//    let result = p baseD 23 i 8
//    printfn "%A" result
//
//    known <- known.Add (key, result)
//
//for i in [0..5] do
//    let key = (baseD, 23, 0, i)
//    printfn "%A" key
//
//    let result = p baseD 23 0 i
//    printfn "%A" result
//
//    known <- known.Add (key, result)