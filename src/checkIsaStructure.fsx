#r "nuget: FsSpreadsheet"
#r "nuget: ISADotNet"
#r "nuget: ISADotNet.Xlsx"

open FsSpreadsheet
open ISADotNet
open ISADotNet.XLSX


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
    match studiesFromInves, studiesFromFiles.Length with
    | None, 0 -> [||]
    | None, n when n > 0 -> [||]
    | Some sfis ->
        sfis
        |> Array.map (
            fun sfi ->
                if sfi
        )
