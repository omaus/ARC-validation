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
    let setSfis =
        match studiesFromInves with
        | Some sfis -> set sfis     // TO DO: URGENT! Add comparison to types Study, Assay and (maybe) Investigation to support sets!
        | None -> set []
    let setSffs = set studiesFromFiles
    setSffs