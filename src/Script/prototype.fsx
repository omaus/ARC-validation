#r "nuget: FSharpAux"
#r "nuget: Expecto"

open FSharpAux
open System.IO
open Expecto

/// Representation of a CWL version as a record type.
type CWLVer = {
    Major: int
    Minor: int
    Patch: int option
}

/// Takes a CWL version as a string and parses it into a CWLVer record.
let private parseCwlVer cwlVer =
    let res = String.split '.' cwlVer
    {
        Major = int res[0]
        Minor = int res[1]
        Patch = try int res[2] |> Some with _ -> None
    }

type Message = {
    Path        : string
    Line        : string
    Position    : string
}

let createMessage path line pos = {Path = path; Line = line; Position = pos}

/// Checks if a given entity is present.
// use this for checking for files, folders, and ISA-related stuff, e.g. if all Source/Sample Names are given etc.
let isPresent actual message = 
    if actual then ()
    else failtestf "Actual entity is not present: %s" message.Path     // <- string hier ist expliziter Fehler (ohne Ort, Ort wird über message realisiert), also Fehlermeldung zum Name der Funktion
    // right now missing here: incorporation of line and position

/// Checks if a given ISA value is registered in the ISA Investigation file.
let isRegistered actual message =
    if actual then ()
    else
    failtestf "Actual value is not registered: %s" message.Path

/// Checks if a given version is valid.
// use this for e.g. CWL version (must be 1.2+)
let isValidVersion actual message =
    let parsedVersion = parseCwlVer actual
    if parsedVersion.Major >= 1 && parsedVersion.Minor >= 2 then ()
    else 
        failtestf "Actual CWL version is below required version 1.2: %s" message.Path

/// Checks if at least one of two given entities are present.
// use this for CWL check: MUST either contain tool description or workflow description
let isEitherPresent actual1 actual2 message =
    if actual1 || actual2 then ()
    else
        failtestf "Neither of the actual entities are present: %s" message.Path

/// Checks if an entity is reproducible.
// use this for checking for Run data reproducibility
let isReproducible actual message =
    if actual then ()
    else
        failtestf "Actual entity is not reproducible: %s" message.Path

/// Checks if an entity is a valid ontological term.
let isValidTerm actual message =
    if actual then ()
    else
        failtestf "Actual entity is not valid: %s" message.Path


// Types:

/// Type representation of a Study folder.
type StudyFolderStructure = {
    Name                : string
    Path                : string
    HasIsaFile          : bool
    HasResourcesFolder  : bool
}

/// Creates a StudyFolderStructure from given parameters.
let createStudyFolderStructure name path hasIsaFile hasResourcesFolder = {
    Name                = name
    Path                = path
    HasIsaFile          = hasIsaFile
    HasResourcesFolder  = hasResourcesFolder
}

/// Type representation of an Assay folder.
type AssayFolderStructure = {
    Name                : string
    Path                : string
    HasIsaFile          : bool
    HasDatasetFolder    : bool
}

/// Creates an AssayFolderStructure from given parameters.
let createAssayFolderStructure name path hasIsaFile hasDatasetFolder = {
    Name                = name
    Path                = path
    HasIsaFile          = hasIsaFile
    HasDatasetFolder    = hasDatasetFolder
}

/// Type representation of a Workflow folder.
type WorkflowFolderStructure = {
    Name                : string
    Path                : string
    HasWorkflowFile     : bool
}

/// Creates a WorkflowFolderStructure from given parameters.
let createWorkflowFolderStructure name path hasWorkflowFile = {
    Name                = name
    Path                = path
    HasWorkflowFile     = hasWorkflowFile
}

/// Type representation of a Run folder.
type RunFolderStructure = {
    Name                : string
    Path                : string
    HasRunFile          : bool
    HasOutputFiles      : bool
}

/// Creates a WorkflowFolderStructure from given parameters.
let createRunFolderStructure name path hasRunFile hasOutputFiles = {
    Name                = name
    Path                = path
    HasRunFile          = hasRunFile
    HasOutputFiles      = hasOutputFiles
}

/// Checks if `path` contains a .git folder and all of its basic files.
let checkForGitFolderStructure gitPath =
    let hooksPath = Path.Combine(gitPath, "hooks")
    let objectsPath = Path.Combine(gitPath, "objects")
    let refsPath = Path.Combine(gitPath, "refs")
    Directory.Exists gitPath                                            &&
    File.Exists (Path.Combine(gitPath, "config"))                       &&
    File.Exists (Path.Combine(gitPath, "description"))                  &&
    File.Exists (Path.Combine(gitPath, "HEAD"))                         &&
    Directory.Exists hooksPath                                          &&
    File.Exists (Path.Combine(hooksPath, "applypatch-msg.sample"))      &&
    File.Exists (Path.Combine(hooksPath, "commit-msg.sample"))          &&
    File.Exists (Path.Combine(hooksPath, "fsmonitor-watchman.sample"))  &&
    File.Exists (Path.Combine(hooksPath, "post-update.sample"))         &&
    File.Exists (Path.Combine(hooksPath, "pre-applypatch.sample"))      &&
    File.Exists (Path.Combine(hooksPath, "pre-commit.sample"))          &&
    File.Exists (Path.Combine(hooksPath, "pre-merge-commit.sample"))    &&
    File.Exists (Path.Combine(hooksPath, "pre-push.sample"))            &&
    File.Exists (Path.Combine(hooksPath, "pre-rebase.sample"))          &&
    File.Exists (Path.Combine(hooksPath, "pre-receive.sample"))         &&
    File.Exists (Path.Combine(hooksPath, "prepare-commit-msg.sample"))  &&
    File.Exists (Path.Combine(hooksPath, "push-to-checkout.sample"))    &&
    File.Exists (Path.Combine(hooksPath, "update.sample"))              &&
    Directory.Exists (Path.Combine(gitPath, "info"))                    &&
    File.Exists (Path.Combine(gitPath, "info", "exclude"))              &&
    Directory.Exists objectsPath                                        &&
    Directory.Exists (Path.Combine(objectsPath, "info"))                &&
    Directory.Exists (Path.Combine(objectsPath, "pack"))                &&
    Directory.Exists refsPath                                           &&
    Directory.Exists (Path.Combine(refsPath, "heads"))                  &&
    Directory.Exists (Path.Combine(refsPath, "tags"))

/// Takes a possible Elements folder path (`string option`) and returns the possible collection of all Elements folders' paths in it. This applies to Studies, Assays, Workflows, and Runs as Elements.
let getElementInElementsFolder elementsFolder =
    match elementsFolder with
    | None -> None
    | Some ef -> Directory.GetDirectories ef |> Some

/// Takes a function specified to transform an input `string` into an Element folder structure and a possible collection of Elements' paths (`string [] option`) and returns the possible folder structure for each Element.
let private checkElementsFolderStructure elemFunction (elementsInElementsfolder : string [] option) =
    match elementsInElementsfolder with
    | None -> None
    | Some elements ->
        elements
        |> Array.map elemFunction
        |> Some

/// Takes a possible collection of Studies' paths (`string [] option`) and returns the possible folder structure of each Study.
let checkStudiesFolderStructure studiesInStudiesFolder =
    let elemFunction dir =
        let n = (DirectoryInfo dir).Name
        let isaf = Path.Combine(dir, "isa.study.xlsx") |> File.Exists
        let rf = Path.Combine(dir, "resources") |> Directory.Exists
        createStudyFolderStructure n dir isaf rf
    checkElementsFolderStructure elemFunction studiesInStudiesFolder

/// Takes a possible collection of Assays' paths (`string [] option`) and returns the possible folder structure of each Assay.
let checkAssaysFolderStructure assaysInAssaysFolder =
    let elemFunction dir =
        let n = (DirectoryInfo dir).Name
        let isaf = Path.Combine(dir, "isa.assay.xlsx") |> File.Exists
        let df = Path.Combine(dir, "dataset") |> Directory.Exists
        createAssayFolderStructure n dir isaf df
    checkElementsFolderStructure elemFunction assaysInAssaysFolder

/// Takes a possible collection of Workflows' paths (`string [] option`) and returns the possible folder structure of each Workflow.
let checkWorkflowsFolderStructure workflowsInWorkflowsFolder =
    let elemFunction dir =
        let n = (DirectoryInfo dir).Name
        let wff = Path.Combine(dir, "workflow.cwl") |> File.Exists
        createWorkflowFolderStructure n dir wff
    checkElementsFolderStructure elemFunction workflowsInWorkflowsFolder

/// Takes a possible collection of Runs' paths (`string [] option`) and returns the possible folder structure of each Run.
let checkRunsFolderStructure runsInRunsFolder =
    let elemFunction dir =
        let n = (DirectoryInfo dir).Name
        let rf = Path.Combine(dir, "run.cwl") |> File.Exists
        let opf = 
            let af = Directory.GetFiles dir
            match af.Length with
            | 0 -> false
            | _ -> af |> Array.exists (fun f -> (FileInfo f).Name <> "run.cwl" )
        createRunFolderStructure n dir rf opf
    checkElementsFolderStructure elemFunction runsInRunsFolder


// Execution:

let inputPath = 
    // this is the path to the ARC
    try System.Environment.GetCommandLineArgs()[0]
    with :? System.IndexOutOfRangeException -> failwith "No or inproper path given."

let arcFolderPath       = Path.Combine(inputPath, ".arc")
let gitFolderPath       = Path.Combine(inputPath, ".git")
let studiesPath         = Path.Combine(inputPath, "studies")
let assaysPath          = Path.Combine(inputPath, "assays")
let runsPath            = Path.Combine(inputPath, "runs")
let workflowsPath       = Path.Combine(inputPath, "workflows")
let investigationPath   = Path.Combine(inputPath, "isa.investigation.xlsx")

let hasArcFolder            = Directory.Exists arcFolderPath
let hasGitFolderStructure   = checkForGitFolderStructure gitFolderPath
let hasStudiesFolder        = Directory.Exists studiesPath
let hasAssaysFolder         = Directory.Exists assaysPath
let hasRunsFolder           = Directory.Exists runsPath
let hasWorkflowsFolder      = Directory.Exists workflowsPath
let hasInvestigationFile    = File.Exists investigationPath
let studiesFolderStructure  = 
    if hasStudiesFolder then getElementInElementsFolder 


let filesystem =
    testList "Filesystem" [
        testCase ".arc" <| fun () -> isPresent hasArcFolder (createMessage arcFolderPath "" "")
        testCase ".git" <| fun () -> isPresent hasGitFolder (createMessage arcFolderPath "" "")
    ]

let isaTests =
    testList "ISA" [
        testCase "Schema" <| fun () ->
            
    ]