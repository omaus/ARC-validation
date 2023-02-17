#r "nuget: FSharpAux"
#r "nuget: Expecto"

open FSharpAux
open Expecto
open Impl
open System
open System.IO
open System.Xml
open System.Xml.Linq
open System.Globalization
open System.Reflection


/// Performs a Test and returns the resulting TestSummary.
let performTest test =
    let w = System.Diagnostics.Stopwatch()
    w.Start()
    evalTests Tests.defaultConfig test
    |> Async.RunSynchronously
    |> fun r -> 
        w.Stop()
        {
            results = r
            duration = w.Elapsed
            maxMemory = 0L
            memoryLimit = 0L
            timedOut = []
        }

// /// Functions for writing test summaries.

let private expectoVersion = 
    // Assembly.GetExecutingAssembly().GetCustomAttribute<AssemblyFileVersionAttribute>()   // fails because of not having the .dll at hand
    let userDir = Environment.SpecialFolder.UserProfile |> Environment.GetFolderPath        // workaround which uses NuGet package version
    Directory.GetDirectories(Path.Combine(userDir, ".nuget", "packages", "expecto"))
    |> Array.last
    |> fun dir -> 
        String.rev dir 
        |> String.takeWhile ((<>) '\\')
        |> String.rev

let private assemblyName = Assembly.GetEntryAssembly().GetName().Name

let private xmlSave fileName (doc : XDocument) =
    let path = Path.GetFullPath fileName
    Path.GetDirectoryName path
    |> Directory.CreateDirectory
    |> ignore
    let settings = XmlWriterSettings(CheckCharacters = false)
    use writer = XmlWriter.Create(path, settings)
    doc.Save writer

// delete when added to FSharpAux
module String =
    /// Returns a new string in which all leading and trailing occurrences of white-space characters from the current string are removed.
    let trim (s : string) = s.Trim()

// Slightly modified from https://github.com/haf/expecto/blob/main/Expecto/TestResults.fs
/// Generate test results using NUnit v2 schema.
let writeNUnitSummary (file : string option) (summary : TestRunSummary) =
    // v3: https://github.com/nunit/docs/wiki/Test-Result-XML-Format
    // this impl is v2: http://nunit.org/docs/files/TestResult.xml
    let totalTests = summary.errored @ summary.failed @ summary.ignored @ summary.passed
    let testCaseElements =
        totalTests
        //|> Seq.sortByDescending (fun (_,test) -> test.result.order, test.duration.TotalSeconds)
        |> Seq.map (fun (flatTest, test) ->
                    
            let fullnameString = 
                flatTest.name 
                |> List.fold (fun acc s -> acc + s + "; " ) "[ "
                |> fun s -> s[.. String.length s - 3] + " ]"
            let element = XElement(XName.Get "test-case", XAttribute(XName.Get "name", fullnameString))
            let addAttribute name (content : string) = element.Add(XAttribute(XName.Get name, content))

            //printfn $"test.result.order is {test.result.order}\ntest.result.tag is {test.result.tag}"

            match test.result with
            | Ignored _ -> "False"
            | _ -> "True"
            |> addAttribute "executed"

            match test.result with
            | Passed -> "Success"
            | Error _
            | Failed _ -> "Failure"
            | Ignored _ -> "Ignored"
            |> addAttribute "result"

            match test.result with
            | Passed -> addAttribute "success" "True"
            | Error _
            | Failed _ -> addAttribute "success" "False"
            // Ignored tests are neither successful nor failed.
            | Ignored _ -> ()

            String.Format(CultureInfo.InvariantCulture, "{0:0.000}", test.duration.TotalSeconds)
            |> addAttribute "time"

            // TODO: implement it.
            addAttribute "asserts" "0"

            let failureNode = XElement(XName.Get "failure")

            // Some more details that explain why a test was not executed.
            match test.result with
            | Passed -> ()
            | Error e ->
                failureNode.Add(XName.Get "message", XCData e.Message)
                //failureNode.Add(XName.Get "stack-trace", XCData e.StackTrace)     // commented out to tackle unnecessary error stack trace in error message
                element.Add failureNode
            | Failed msg ->
                // added to tackle unnecessary error stack trace in failure message
                let eWithoutStackTrace =
                    String.trim msg
                    |> String.split '\n'
                    |> Array.head
                //failureNode.Add(XName.Get "message", XCData msg)
                failureNode.Add(XName.Get "message", XCData eWithoutStackTrace)
                element.Add failureNode
            | Ignored msg -> element.Add(XElement(XName.Get "reason", XElement(XName.Get "message", XCData msg)))
            element)
    let d = DateTime.Now
    let xAttr name data = XAttribute(XName.Get name, data)
    let element =
        XElement(XName.Get "test-results",
            xAttr "date" (d.ToString("yyyy-MM-dd")),
            xAttr "name" assemblyName,
            xAttr "total" totalTests.Length,
            xAttr "errors" summary.errored.Length,
            xAttr "failures" summary.failed.Length,
            xAttr "ignored" summary.ignored.Length,
            xAttr "not-run" "0",
            xAttr "inconclusive" "0",
            xAttr "skipped" "0",
            xAttr "invalid" "0",
            xAttr "time" (d.ToString("HH:mm:ss")),
            XElement(XName.Get "environment",
                xAttr "expecto-version" expectoVersion,
                xAttr "clr-version" Environment.Version,
                xAttr "os-version" Environment.OSVersion.VersionString,
                xAttr "platform" Environment.OSVersion.Platform,
                xAttr "cwd" Environment.CurrentDirectory,
                xAttr "machine-name" Environment.MachineName,
                xAttr "user" Environment.UserName,
                xAttr "user-domain" Environment.UserDomainName
            ),
            XElement(XName.Get "culture-info",
                xAttr "current-culture", CultureInfo.CurrentCulture,
                xAttr "current-uiculture", CultureInfo.CurrentUICulture
            ),
            XElement(XName.Get "test-suite",
                xAttr "type" "Assembly",
                xAttr "name" assemblyName,
                xAttr "executed" "True",
                xAttr "result" (if summary.successful then "Success" else "Failure"),
                xAttr "success" (if summary.successful then "True" else "False"),
                xAttr "time"
                    (String.Format(CultureInfo.InvariantCulture,
                        "{0:0.000}", summary.duration.TotalSeconds)),
                xAttr "asserts" "0",
                XElement(XName.Get "results", testCaseElements)
            )
        )
    
    let xDoc = XDocument element

    match file with
    | Some path -> xmlSave path xDoc
    | None      -> ()

    xDoc.ToString()

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
    try 
        let l = Environment.GetCommandLineArgs().Length
        Environment.GetCommandLineArgs()[l - 2]
    with :? IndexOutOfRangeException -> failwith "No or inproper input path given."

let outputPath = 
    // this is the path to the testResult file
    try 
        let l = Environment.GetCommandLineArgs().Length
        Environment.GetCommandLineArgs()[l - 1]
    with :? IndexOutOfRangeException -> failwith "No or inproper summary file path given."
    |> Some 

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


let filesystem =
    testList "Filesystem" [
        testCase ".arc" <| fun () -> isPresent hasArcFolder (createMessage arcFolderPath "" "")
    ]

let res = performTest filesystem
writeNUnitSummary outputPath res