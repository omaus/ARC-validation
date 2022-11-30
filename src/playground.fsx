#load "checkArcStructure.fsx"
#load "checkIsaStructure.fsx"
#r "nuget: Expecto"
#r "nuget: ISADotNet.XLSX"

open CheckArcStructure
open CheckIsaStructure
open Expecto
open Expecto.Expect
open ISADotNet.XLSX
open System.IO

let pathToCheck = System.Environment.GetCommandLineArgs()[0]

if Directory.Exists pathToCheck |> not then failwith "Input argument is no path to validate."

let hasArcFolder            = checkForArcFolder             pathToCheck
let hasGitFolderStructure   = checkForGitFolderStructure    pathToCheck
let hasStudiesFolder        = checkForStudiesFolder         pathToCheck
let hasAssaysFolder         = checkForAssaysFolder          pathToCheck
let hasInvestigationFile    = checkForInvestigationFile     pathToCheck

let studiesFolder           = if hasStudiesFolder   then Path.Combine(pathToCheck, "studies")   |> Some else None
let assaysFolder            = if hasAssaysFolder    then Path.Combine(pathToCheck, "assays")    |> Some else None
let studiesInStudiesFolder  = getElementInElementsFolder studiesFolder
let assaysInAssaysFolder    = getElementInElementsFolder assaysFolder

let investigation = Investigation.fromFile (Path.Combine(pathToCheck, "isa.investigation.xlsx"))
let studies =
    match studiesInStudiesFolder with
    | Some ssfs -> ssfs |> Array.map StudyFile.Study.fromFile |> List.ofArray
    | None      -> []
let assays =
    match assaysInAssaysFolder with
    | Some aafs -> aafs |> Array.map (AssayFile.Assay.fromFile >> snd) |> List.ofArray
    | None      -> []

let studyRegistration   = checkStudiesRegistration studies investigation
let assayRegistration   = checkAssaysRegistration assays investigation

let fileStructureTests = testList "ARC filesystem structure tests" [
    testList "Folder structure tests" [
        testCase "Has .arc folder"              <| fun () -> isTrue hasArcFolder            ".arc folder does not exist"
        testCase "Has Git folder structure"     <| fun () -> isTrue hasGitFolderStructure   "Git folder structure does not exist"
        testCase "Has Studies folder"           <| fun () -> isTrue hasStudiesFolder        "Studies folder does not exist"
        testCase "Has Assays folder"            <| fun () -> isTrue hasAssaysFolder         "Assays folder does not exist"
    ]
    testList "File structure tests" [
        testCase "Has ISA Investigation file"   <| fun () -> isTrue hasInvestigationFile    "ISA Investigation file does not exist"
    ]
]

let isaStructureTests = testList "ISA structure tests" [
    testList "ISA Study tests" [
        testCase "All studies are registered"   <| fun () -> 
            let missingStudiesNames = studyRegistration.MissingStudies |> List.map (fun s -> s.ID)
            let protoMsg = sprintf "Not all studies are registered.\nStudies %s are missing in the study folder. Studies %s"
            isTrue studyRegistration.AreStudiesRegistered "Not all st"
    ]
    testList "ISA Assay tests" [
        testCase "All assays are registered"    <| fun () -> isTrue
    ]
]

isTrue true ".arc folder does exist"