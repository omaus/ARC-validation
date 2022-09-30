#r "nuget: FsSpreadsheet"
#r "nuget: ISADotNet"
#r "nuget: ISADotNet.Xlsx"

open FsSpreadsheet
open ISADotNet
open ISADotNet.XLSX

let speedTest count func =
    let sw = System.Diagnostics.Stopwatch()
    for i = 1 to count do
        printfn $"Run {i} of {count}"
        sw.Start()
        func () |> ignore
        sw.Stop()
    sw.Elapsed

let multiSpeedTest count funcs = funcs |> Seq.map (speedTest count) |> Array.ofSeq

let multiSpeedTestAsync funcs count =
    funcs
    |> Seq.map (fun func -> async {return speedTest count func})
    |> Async.Parallel
    |> Async.RunSynchronously

module Seq =
    /// Computes the intersection of two sequences.
    let intersect1 (seq1 : seq<'T>) seq2 = 
        let smallerSeq, largerSeq =
            if Seq.length seq1 >= Seq.length seq2 then seq2, seq1
            else seq1, seq2
        let hs = System.Collections.Generic.HashSet<'T>(HashIdentity.Structural<'T>)    // for distinction
        smallerSeq |> Seq.filter (fun e -> Seq.contains e largerSeq && hs.Add e)

    /// Computes the outersection (known as "symmetric difference" in mathematics) of two sequences.
    let outersect (seq1 : seq<'T>) seq2 = seq {
        let hs = System.Collections.Generic.HashSet<'T>(HashIdentity.Structural<'T>)    // for distinction
        for e in seq1 do if hs.Add e && Seq.contains e seq2 |> not then e
        for e in seq2 do if hs.Add e && Seq.contains e seq1 |> not then e
    }

module Array =
    /// Computes the intersection of two arrays.
    let intersect (arr1 : 'T []) (arr2 : 'T []) =
        let smallerArr, largerArr =
            if arr1.Length >= arr2.Length then arr2, arr1
            else arr1, arr2
        let hs = System.Collections.Generic.HashSet<'T>(HashIdentity.Structural<'T>)    // for distinction
        smallerArr |> Array.filter (fun e -> hs.Add e && Array.contains e largerArr)

    /// Computes the outersection (known as "symmetric difference" in mathematics) of two arrays.
    let outersect arr1 arr2 = [|
        let hs = System.Collections.Generic.HashSet<'T>(HashIdentity.Structural<'T>)    // for distinction
        yield! arr1 |> Array.filter (fun e -> hs.Add e && Array.contains e arr2 |> not)
        yield! arr2 |> Array.filter (fun e -> hs.Add e && Array.contains e arr1 |> not)
    |]

module List =
    /// Computes the intersection of two lists.
    let intersect (list1 : 'T list) (list2 : 'T list) =
        let smallerList, largerList =
            if list1.Length >= list2.Length then list2, list1
            else list1, list2
        let hs = System.Collections.Generic.HashSet<'T>(HashIdentity.Structural<'T>)    // for distinction
        let rec loop predicate l1 l2 fl =
            match l1 with
            | h :: t -> if hs.Add h && predicate h l2 then loop predicate t l2 (h :: fl) else loop predicate t l2 fl
            | [] -> fl
        loop List.contains smallerList largerList []
        |> List.rev

    /// Computes the outersection (known as "symmetric difference" in mathematics) of two lists.
    let outersect (list1 : 'T list) (list2 : 'T list) = 
        let smallerList, largerList =
            if list1.Length >= list2.Length then list2, list1
            else list1, list2
        let hs = System.Collections.Generic.HashSet<'T>(HashIdentity.Structural<'T>)
        let hsDepr = System.Collections.Generic.HashSet<'T>(HashIdentity.Structural<'T>)
        let rec loop l1 l2 fl =
            match l1 with
            | h :: t -> 
                if hs.Add h then 
                    if List.contains h l2 |> not then
                        loop t l2 (h :: fl)
                    else 
                        hsDepr.Add h |> ignore
                        loop t l2 fl
                else loop t l2 fl
            | [] -> fl
        let os1 = loop smallerList largerList [] |> List.rev
        let lDepr = Seq.toList hsDepr
        let os2 = loop largerList lDepr [] |> List.rev
        [yield! os1; yield! os2]


/// Checks if all existing Studies are registered in the Investigation file and if all registered Studies in the Investigation file are present in the ARC.
let areStudiesRegistered studiesPaths invesPath =
    let inves = Investigation.fromFile invesPath
    let studiesFromInves = inves.Studies
    let studiesFromFiles = 
        match studiesPaths with
        | None -> [||]
        | Some sps ->
            sps
            |> Array.map StudyFile.Study.fromFile
    let setSfis =
        match studiesFromInves with
        | Some sfis -> set sfis
        | None -> set []
    let setSffs = set studiesFromFiles
    setSffs