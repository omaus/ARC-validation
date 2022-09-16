#load "checkArcStructure.fsx"
#load "checkIsaFiles.fsx"

#r "nuget: Expecto"

open CheckArcStructure

open Expecto
open Expecto.Expect

open System.IO


// Pre-stuff:

let pathToCheck = System.Environment.GetCommandLineArgs()[0]

if Directory.Exists pathToCheck |> not then failwith "Input argument is no path to validate."


// Evaluation (Checks):

let hasArcFolder = checkForArcFolder pathToCheck

let hasGitFolderStructure = checkForGitFolderStructure pathToCheck

let hasStudiesFolder = checkForStudiesFolder pathToCheck

let hasAssaysFolder = checkForAssaysFolder pathToCheck

let studiesFolder = if hasStudiesFolder then Path.Combine(pathToCheck, "studies") |> Some else None

let assaysFolder = if hasAssaysFolder then Path.Combine(pathToCheck, "assays") |> Some else None

let hasRunsFolder = checkForRunsFolder pathToCheck

let hasWorkflowsFolder = checkForWorkflowsFolder pathToCheck

let hasInvestigationFile = checkForInvestigationFile pathToCheck

let studiesInStudiesFolder = getElementInElementsFolder studiesFolder

let assaysInAssaysFolder = getElementInElementsFolder assaysFolder

let studiesFolderStructure = checkStudiesFolderStructure studiesInStudiesFolder

let assaysFolderStructure = checkAssaysFolderStructure assaysInAssaysFolder


// Validation (Tests):

let fileStructureTests = testList "File structure tests" [
    testCase "Has .arc folder" <| fun () -> isTrue hasArcFolder ".arc folder does exist"
    testCase "Has Git folder structure" <| fun () -> isTrue hasGitFolderStructure "Git folder structure does exist"
    testCase "Has Studies folder" <| fun () -> isTrue hasStudiesFolder "Studies folder does exist"
    testCase "Has Assays folder" <| fun () -> isTrue hasAssaysFolder "Assays folder does exist"
    testCase "Has Runs folder" <| fun () -> isTrue hasRunsFolder "Runs folder does exist"
    testCase "Has Workflows folder" <| fun () -> isTrue hasWorkflowsFolder "Workflows folder does exist"
    testCase "Has ISA Investigation file" <| fun () -> isTrue hasInvestigationFile "ISA Investigation file does exist"
    match studiesFolderStructure with
    | None -> ()
    | Some studies ->
        studies
        |> Array.map (
            fun s ->
                testList $"Tests for Study {s.Name}" [
                    testCase $"Study {s.Name} has ISA study file" <| fun () -> isTrue s.HasIsaFile "ISA Study file does exist"
                    testCase $"Study {s.Name} has Resources folder" <| fun () -> isTrue s.HasResourcesFolder "Resources folder does exist"
                    testCase $"Study {s.Name} has Protocols folder" <| fun () -> isTrue s.HasProtocolsFolder "Protocols folder does exist"
                ]
        )
        |> Array.toList
        |> testList "Individual Studies tests"
    match assaysFolderStructure with
    | None -> ()
    | Some assays ->
        assays
        |> Array.map (
            fun a ->
                testList $"Tests for Assay {a.Name}" [
                    testCase $"Assay {a.Name} has ISA assay file" <| fun () -> isTrue a.HasIsaFile "ISA Assay file does exist"
                    testCase $"Assay {a.Name} has Dataset folder" <| fun () -> isTrue a.HasDatasetFolder "Dataset folder does exist"
                    testCase $"Assay {a.Name} has Protocols folder" <| fun () -> isTrue a.HasProtocolsFolder "Protocols folder does exist"
                ]
        )
        |> Array.toList
        |> testList "Individual Assays tests"
]

let isaStandardTests = testList "ISA standard tests" [
    
]

let testsCombined = testList "Tests combined" [fileStructureTests]