#load "load.fsx"

open Aardvark.Base
open System.Runtime.CompilerServices
open Aardvark.Base.Rendering
open Microsoft.FSharp.Quotations
open Aardvark.Rendering.Vulkan
open System.Runtime.InteropServices

open Aardvark.Base.Incremental

type H = Div of alist<H>
       | T of string
       | TI of IMod<string>

type S = SDiv of plist<S>
       | ST of string


//let rec createState (h : H) : S =
//    match h with
//        | Div a -> SDiv (a |> AList.toList |> List.map createState)
//        | T s -> ST s

let rec div (os : S) (ns : S) : list<string> = 
    match os,ns with
        | SDiv a, SDiv b when PList.count a = PList.count b -> 
            let a' = PList.toList a
            let b' = PList.toList b
            List.zip a' b' |> List.map (fun (a,b) -> div a b) |> List.concat
        | ST l, ST r -> 
            if l = r then [] else [sprintf "replace %s with %s" l r]
        | _ -> [sprintf "remove %A, add %A" os ns]


let cache = Dict<list<Index>,obj>()
let rec mapState (path : list<Index>) (h' : H) : IMod<S> =
    Mod.custom (fun token -> 
        match h' with
            | T s -> ST s
            | Div xs -> 
                printfn "mk reader: %A" xs
                let reader = dictxs.GetReader()
                let ops = reader.GetOperations(token)
                let mapped = PList.mapi (fun i a -> (mapState (i :: path) a).GetValue token) reader.State
                SDiv mapped
            | TI ms -> ST (ms.GetValue token)
    )

let test () =
    let ss1 = CList.ofSeq [ TI (Mod.init "asdfa")]
    let ta1 = CList.ofSeq [Div ss1]
    let ss2 = CList.ofSeq [ TI (Mod.init "heheh")]
    let ta2 = CList.ofSeq [Div ss2]

    let cl = CList.ofSeq [T "a"; T "b"; Div ta1]
    let t = Div cl

    let s = mapState [] t

    let s1 = s |> Mod.force

    transact (fun _ -> 
        //ta.Clear()
        cl.RemoveAt 2
        cl.Append (Div ta2)

    ) |> ignore

    let s2 = s |> Mod.force 
    printfn "%A" s2

    let d = div s1 s2
    printfn "%A" d