#load "checkArcStructure.fsx"
#load "checkIsaFiles.fsx"

#r "nuget: Expecto"

open CheckArcStructure

open Expecto

open System.IO


// Types:

type StudyFolderStructure = {
    Name                : string
    Path                : string
    HasIsaFile          : bool
    HasResourcesFolder  : bool
    HasProtocolsFolder  : bool
}

let createStudyFolderStructure name path hasIsaFile hasResourcesFolder hasProtocolsFolder = {
    Name                = name
    Path                = path
    HasIsaFile          = hasIsaFile
    HasResourcesFolder  = hasResourcesFolder
    HasProtocolsFolder  = hasProtocolsFolder
}

type AssayFolderStructure = {
    Name                : string
    Path                : string
    HasIsaFile          : bool
    HasDatasetFolder    : bool
    HasProtocolsFolder  : bool
}

let createAssayFolderStructure name path hasIsaFile hasDatasetFolder hasProtocolsFolder = {
    Name                = name
    Path                = path
    HasIsaFile          = hasIsaFile
    HasDatasetFolder    = hasDatasetFolder
    HasProtocolsFolder  = hasProtocolsFolder
}


// Pre-stuff:

let pathToCheck = System.Environment.GetCommandLineArgs()[0]

if Directory.Exists pathToCheck |> not then failwith "Input argument is no path to validate."

let studiesFolder = Path.Combine(pathToCheck, "studies")

let assaysFolder = Path.Combine(pathToCheck, "assays")


// Evaluation (Checks):

let hasArcFolder = checkForArcFolder pathToCheck

let hasGitFolderStructure = checkForGitFolderStructure pathToCheck

let hasStudiesFolder = checkForAssayFolder pathToCheck

let hasAssayFolder = checkForAssayFolder pathToCheck

let hasRunsFolder = checkForRunsFolder pathToCheck

let hasWorkflowsFolder = checkForWorkflowsFolder pathToCheck

let hasInvestigationFile = checkForInvestigationFile pathToCheck

let studiesInStudiesFolder =
    if hasStudiesFolder then Directory.GetDirectories studiesFolder |> Some
    else None

let assaysInAssaysFolder =
    if hasAssayFolder then Directory.GetDirectories assaysFolder |> Some
    else None

let assaysFolderStructure =
    match assaysInAssaysFolder with
    | None -> None
    | Some assays ->
        assays
        |> Array.map (
            fun dir ->
                let p = Path.Combine(dir, "isa.xlsx")
                let n = (DirectoryInfo dir).Name
                let 
        )


// Validation (Tests):