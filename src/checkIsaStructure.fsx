#r "nuget: FsSpreadsheet"
#r "nuget: ISADotNet"
#r "nuget: ISADotNet.Xlsx"

open FsSpreadsheet
open ISADotNet
open ISADotNet.XLSX

module Seq =
    /// Computes the intersection of two sequences.
    let intersect (seq1 : seq<'T>) seq2 = seq {
        let smallerSeq, largerSeq =
            if Seq.length seq1 >= Seq.length seq2 then seq2, seq1
            else seq1, seq2
        for e in smallerSeq do
            if Seq.contains e largerSeq then e
    }

    /// Computes the outersection (known as "symmetric difference" in mathematics) of two sequences.
    let outersect1 (seq1 : seq<'T>) seq2 = seq {
        //for e in seq1 do
        //    if Seq.contains e seq2 |> not then e
        //for e in seq2 do
        //    if Seq.contains e seq1 |> not then e
        let bothSeqs = seq {yield! seq1; yield! seq2} |> Seq.distinct
        for e in bothSeqs do
            if Seq.contains e seq1 && Seq.contains e seq2 |> not then e
    }

    let outersect2 (seq1 : seq<'T>) seq2 = seq {
        //for e in seq1 do
        //    if Seq.contains e seq2 |> not then e
        //for e in seq2 do
        //    if Seq.contains e seq1 |> not then e
        let bothSeqs = seq {yield! seq1; yield! seq2}
        for e in bothSeqs do
            if Seq.contains e seq1 && Seq.contains e seq2 |> not then e
    }

module Array =
    /// Computes the intersection of two arrays.
    let intersect (arr1 : 'T []) (arr2 : 'T []) =
        let smallerArr, largerArr =
            if arr1.Length >= arr2.Length then arr2, arr1
            else arr1, arr2
        smallerArr
        |> Array.filter (fun e -> Array.contains e largerArr)

module List =
    /// Computes the intersection of two lists.
    let intersect (list1 : 'T list) (list2 : 'T list) =
        let smallerList, largerList =
            if list1.Length >= list2.Length then list2, list1
            else list1, list2
        let rec loop predicate l1 l2 fl =
            match l1 with
            | h :: t -> if predicate h l2 then loop predicate t l2 (h :: fl) else loop predicate t l2 fl
            | [] -> fl
        loop List.contains smallerList largerList []

// TO DO: Add outersection functions to modules!

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