#r "nuget: FsSpreadsheet"
#r "nuget: ISADotNet"
#r "nuget: ISADotNet.Xlsx"

open FsSpreadsheet
open ISADotNet
open ISADotNet.XLSX

open System.Collections.Generic

module Seq =
    /// Computes the intersection of two sequences.
    let intersect (seq1 : seq<'T>) seq2 : seq<'T> = 
        let smallerSeq, largerSeq =
            if Seq.length seq1 >= Seq.length seq2 then seq2, seq1
            else seq1, seq2
        let hsSs = HashSet<'T>(HashIdentity.Structural<'T>)
        smallerSeq |> Seq.iter (hsSs.Add >> ignore)
        hsSs.IntersectWith largerSeq
        hsSs

    /// Computes the outersection (known as "symmetric difference" in mathematics) of two sequences.
    let outersect seq1 (seq2 : seq<'T>) : seq<'T> = 
        let hsS1 = HashSet<'T>(HashIdentity.Structural<'T>)
        seq1 |> Seq.iter (hsS1.Add >> ignore)
        hsS1.SymmetricExceptWith seq2
        hsS1

module Array =
    /// Computes the intersection of two arrays.
    let intersect (arr1 : 'T []) (arr2 : 'T []) =
        let smallerArr, largerArr =
            if arr1.Length >= arr2.Length then arr2, arr1
            else arr1, arr2
        let hsSa = HashSet<'T>(HashIdentity.Structural<'T>)
        smallerArr |> Array.iter (hsSa.Add >> ignore)
        hsSa.IntersectWith largerArr
        Array.ofSeq hsSa

    /// Computes the outersection (known as "symmetric difference" in mathematics) of two arrays.
    let outersect arr1 (arr2 : 'T []) =
        let hsS1 = HashSet<'T>(HashIdentity.Structural<'T>)
        arr1 |> Array.iter (hsS1.Add >> ignore)
        hsS1.SymmetricExceptWith arr2
        Array.ofSeq hsS1

module List =
    /// Computes the intersection of two lists.
    let intersect (list1 : 'T list) (list2 : 'T list) =
        let smallerList, largerList =
            if list1.Length >= list2.Length then list2, list1
            else list1, list2
        let hsSl = HashSet<'T>(HashIdentity.Structural<'T>)
        smallerList |> List.iter (hsSl.Add >> ignore)
        hsSl.IntersectWith largerList
        List.ofSeq hsSl

    /// Computes the outersection (known as "symmetric difference" in mathematics) of two lists.
    let outersect (list1 : 'T list) (list2 : 'T list) = 
        let hsS1 = HashSet<'T>(HashIdentity.Structural<'T>)
        list1 |> List.iter (hsS1.Add >> ignore)
        hsS1.SymmetricExceptWith list2
        List.ofSeq hsS1

/// Checks if all existing Studies are registered in the Investigation file and if all registered Studies in the Investigation file are present in the ARC.
let areStudiesRegistered studiesPaths invesPath =
    let inves = Investigation.fromFile invesPath
    let studiesFromInves = 
        match inves.Studies with
        | Some sfs -> sfs
        | None -> []
    let studiesFromFiles = 
        match studiesPaths with
        | None -> [||]
        | Some sps ->
            sps
            |> Array.map StudyFile.Study.fromFile
    if 