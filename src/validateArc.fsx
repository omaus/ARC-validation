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

let hasArcFolder            = checkForArcFolder             pathToCheck
let hasGitFolderStructure   = checkForGitFolderStructure    pathToCheck
let hasStudiesFolder        = checkForStudiesFolder         pathToCheck
let hasAssaysFolder         = checkForAssaysFolder          pathToCheck
let hasWorkflowsFolder      = checkForWorkflowsFolder       pathToCheck
let hasRunsFolder           = checkForRunsFolder            pathToCheck
let hasInvestigationFile    = checkForInvestigationFile     pathToCheck

let studiesFolder           = if hasStudiesFolder   then Path.Combine(pathToCheck, "studies")   |> Some else None
let assaysFolder            = if hasAssaysFolder    then Path.Combine(pathToCheck, "assays")    |> Some else None
let workflowsFolder         = if hasWorkflowsFolder then Path.Combine(pathToCheck, "workflows") |> Some else None
let runsFolder              = if hasRunsFolder      then Path.Combine(pathToCheck, "runs")      |> Some else None

let studiesInStudiesFolder      = getElementInElementsFolder studiesFolder
let assaysInAssaysFolder        = getElementInElementsFolder assaysFolder
let workflowsInWorkflowsFolder  = getElementInElementsFolder workflowsFolder
let runsInRunsFolder            = getElementInElementsFolder runsFolder

let studiesFolderStructure      = checkStudiesFolderStructure   studiesInStudiesFolder
let assaysFolderStructure       = checkAssaysFolderStructure    assaysInAssaysFolder
let workflowsFolderStructure    = checkWorkflowsFolderStructure workflowsInWorkflowsFolder
let runsFolderStructure         = checkRunsFolderStructure      runsInRunsFolder


// Validation (Tests):

/// Takes an Element type's name, a function to create `TestCase`s of this Element type and the Element's folder structure to return a list of Tests for validation.
let inline validateElementFolderStructure (elemType : string) createTestCases (elementFolderStructure : 'a [] option) =
    let inline getName e = (^a : (member Name : string) e)
    [
        match elementFolderStructure with
        | Some elements ->
            elements
            |> Array.map (fun e -> testList $"Tests for {elemType} {getName e}" (createTestCases e))
            |> Array.toList
            |> testList $"Individual {elemType} tests" 
        | None -> ()
    ]

/// Takes a Study folder structure and returns a list of TestCases based on it.
let getStudyTestCases (s : StudyFolderStructure) = [
    testCase $"Study {s.Name} has ISA study file"   <| fun () -> isTrue s.HasIsaFile            "ISA Study file does exist"
    testCase $"Study {s.Name} has Resources folder" <| fun () -> isTrue s.HasResourcesFolder    "Resources folder does exist"
]
/// Takes an Assay folder structure and returns a list of TestCases based on it.
let getAssayTestCases (a : AssayFolderStructure) = [
    testCase $"Assay {a.Name} has ISA assay file"   <| fun () -> isTrue a.HasIsaFile          "ISA Assay file does exist"
    testCase $"Assay {a.Name} has Dataset folder"   <| fun () -> isTrue a.HasDatasetFolder    "Dataset folder does exist"
]
/// Takes a Workflow folder structure and returns a list of TestCases based on it.
let getWorkflowTestCases (w : WorkflowFolderStructure) = [
    testCase $"Workflow {w.Name} has CWL Workflow file" <| fun () -> isTrue w.HasWorkflowFile "CWL Workflow file does exist"
]
/// Takes a Run folder structure and returns a list of TestCases based on it.
let getRunsTestCases (r : RunFolderStructure) = [
    testCase $"Run {r.Name} has CWL Run file"   <| fun () -> isTrue r.HasRunFile        "CWL Run file does exist"
    testCase $"Run {r.Name} has output file(s)" <| fun () -> isTrue r.HasOutputFiles    "Output file(s) do(es) exist"
]

let fileStructureTests = testList "File structure tests" [
    testCase "Has .arc folder"              <| fun () -> isTrue hasArcFolder            ".arc folder does exist"
    testCase "Has Git folder structure"     <| fun () -> isTrue hasGitFolderStructure   "Git folder structure does exist"
    testCase "Has Studies folder"           <| fun () -> isTrue hasStudiesFolder        "Studies folder does exist"
    testCase "Has Assays folder"            <| fun () -> isTrue hasAssaysFolder         "Assays folder does exist"
    testCase "Has Workflows folder"         <| fun () -> isTrue hasWorkflowsFolder      "Workflows folder does exist"
    testCase "Has Runs folder"              <| fun () -> isTrue hasRunsFolder           "Runs folder does exist"
    testCase "Has ISA Investigation file"   <| fun () -> isTrue hasInvestigationFile    "ISA Investigation file does exist"
    yield! (validateElementFolderStructure "Study"      getStudyTestCases       studiesFolderStructure      )
    yield! (validateElementFolderStructure "Assay"      getAssayTestCases       assaysFolderStructure       )
    yield! (validateElementFolderStructure "Workflow"   getWorkflowTestCases    workflowsFolderStructure    )
    yield! (validateElementFolderStructure "Runs"       getRunsTestCases        runsFolderStructure         )
]

let isaStandardTests = testList "ISA standard tests" [
    
]

let testsCombined = testList "Tests combined" [fileStructureTests]