#r "nuget: Expecto"
#r "nuget: FSharpAux"

open Expecto
open FSharpAux

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

// thought it's easier to represent this as a simple string
// /// Record type representation of a Filesystem entry (i.e., a folder or a file) message, containing its path.
//type FileSystemEntityMessage = {
//    Path    : string
//}

/// Record type representation of a text file message, containing its path and the line and position.
type TextFileMessage = {
    Path    : string
    Line    : string
    Pos     : string
}

/// Record type representation of an XLSX file message, containing its path and the cell reference.
type XlsxFileMessage = {
    Path    : string
    Cell    : string
}

/// Union type representation of the different messages that can be used for describing the cause of a failed test.
type Message =
    /// Representation of a Filesystem entry (i.e., a folder or a file) message.
    | FileSystemEntityMessage   of string
    /// Representation of a text file message.
    | TextFileMessage           of TextFileMessage
    /// Representation of an XLSX file message.
    | XlsxFileMessage           of XlsxFileMessage

/// Takes a Message and returns a string, containing its information.
let parseMessage message =
    match message with
    | FileSystemEntityMessage   s   -> $"Path: {s}."
    | TextFileMessage           tfm -> $"Path: {tfm.Path}, Line: {tfm.Line}, Position: {tfm.Pos}."
    | XlsxFileMessage           xfm -> $"Path: {xfm.Path}, Cell: {xfm.Cell}."

/// Functions for building ARC-specific Expecto-Unit Tests.
module Build =

    /// Top level for creating ARC-specific Test lists.
    module Toplevel =
        /// Builds an ISA-related Test list.
        let isa = testList "ISA"
        /// Builds an ARC Filesystem-related Test list.
        let filesystem = testList "Filesystem"

    /// First sublevel for creating ARC-specific Test lists.
    module Sublevel1 =
        /// Builds an ISA Schema-related Test list.
        let schema = testList "Schema"    // alt.: let content = testList "Content"
        /// Builds an ISA Semantic-related Test list.
        let semantic = testList "Semantic"
        /// Builds an ISA Plausibility-related Test list.
        let plausibility = testList "Plausibility"
    
    /// Second sublevel for creating ARC-specific Test lists.
    module Sublevel2 =
        /// Builds an ISA Study-related Test list.
        let study = testList "Study"
        /// Builds an ISA Assay-related Test list.
        let assay = testList "Assay"
        /// Builds an ARC CWL-related Test list.
        let arcCwl = testList "CWL"
        /// Builds an Workflow-related Test list.
        let workflow = testList "Workflow"
        /// Builds an Run-related Test list.
        let run = testList "Run"

    /// Case level for creating ARC-specific Test cases.
    module CaseLevel =
        
        // Filesystem
        /// Builds an Test case regarding the ARC .arc folder.
        let arcFolder = testCase ".arc"
        /// Builds an Test case regarding the ARC .git folder.
        let gitFolder = testCase ".git"
        /// Builds an Test case regarding the ARC Studies folder.
        let studies = testCase "Studies"
        /// Builds an Test case regarding the ARC Assays folder.
        let assays = testCase "Assays"
        /// Builds an Test case regarding the ARC Workflows folder.
        let workfows = testCase "Workflows"
        /// Builds an Test case regarding the ARC Runs folder.
        let runs = testCase "Runs"
        /// Builds an ISA Study file-related Test case.
        let study = testCase "Study"
        /// Builds an ISA Assay file-related Test case.
        let assay = testCase "Assay"
        /// Builds an ISA Investigation file-related Test case.
        let investigation = testCase "Investigation"
        /// Builds a Test case regarding the ARC CWL file.
        let arcCwl = testCase "ARC CWL"
        /// Builds a Test case regarding the Run file of a Workflow.
        let workflowRun = testCase "Run of a Workflow"

        // ISA
        /// Builds a Test case regarding the Sample Name column or a column that has the same function, i.e., Raw Data File and Derived Data File.
        let sampleNameColumn = testCase "SampleNameColumn"
        /// Builds a Test case regarding the Source Name column.
        let sourceNameColumn = testCase "SourceNameColumn"
        /// Builds a Test case regarding the version of a CWL file.
        let cwlVersion = testCase "CWL version"
        /// Builds a Test case regarding the description of a Tool and/or a Workflow of a CWL file.
        let cwlDescription = testCase "CWL Tool/Workflow description"
        /// Builds a Test case regarding the Metadata sheet.
        let metadata = testCase "Metadata"
        /// Builds an ISA Factor-related Test case.
        let factor = testCase "Factor"
        /// Builds an ontology term-related Test case.
        let term = testCase "Term"

/// Functions for checking ARC- or ISA-specific properties.
module Check =

    (* ISA MUSTs: 
        - all Studies/Assays registered
        - CWLs MUST be v1.2+
        - Assay: metadata section (worksheet) MUST be present
        - Study: metadata section (worksheet) MUST be present (only the 5 definite Study metadata headers)
        - Swate tables: `Source Name` column MUST be present, `Sample Name`/`Raw Data File`/`Derived Data File` too
        - CWLs MUST either contain tool description or workflow description
        - if Workflows are present, there MUST be parallel Run files accordingly
    MAYs: (publishability)
        - MUST contain an Assay or Workflow
        - Investigation sections MUST be filled: Identifier, Title, Description, Contacts
        - MUST be reproducible (i.e. Run data MUST be reproducible)
    *)

    /// Checks if a given entity is present.
    // use this for checking for files, folders, and ISA-related stuff, e.g. if all Source/Sample Names are given etc.
    let isPresent actual message = 
        if actual then ()
        else failtestf "Actual entity is not present: %s" (parseMessage message)     // <- string hier ist expliziter Fehler (ohne Ort, Ort wird über message realisiert), also Fehlermeldung zum Name der Funktion

    /// Checks if a given ISA value is registered in the ISA Investigation file.
    let isRegistered actual message =
      if actual then ()
      else
        failtestf "Actual value is not registered: %s" (parseMessage message)

    /// Checks if a given version is valid.
    // use this for e.g. CWL version (must be 1.2+)
    let isValidVersion actual message =
        let parsedVersion = parseCwlVer actual
        if parsedVersion.Major >= 1 && parsedVersion.Minor >= 2 then ()
        else 
            failtestf "Actual CWL version is below required version 1.2: %s" (parseMessage message)

    /// Checks if at least one of two given entities are present.
    // use this for CWL check: MUST either contain tool description or workflow description
    let isEitherPresent actual1 actual2 message =
        if actual1 || actual2 then ()
        else
            failtestf "Neither of the actual entities are present: %s" (parseMessage message)

    /// Checks if an entity is reproducible.
    // use this for checking for Run data reproducibility
    let isReproducible actual message =
        if actual then ()
        else
            failtestf "Actual entity is not reproducible: %s" (parseMessage message)

    /// Checks if an entity is a valid ontological term.
    let isValidTerm actual message =
        if actual then ()
        else
            failtestf "Actual entity is not valid: %s" (parseMessage message)


open Build
open Build.Toplevel
open Build.Sublevel1
open Build.Sublevel2
open Build.CaseLevel
open Check

let studyXlsx = {Path = "bla"; Cell = "A1"}
let studySourceNameColumn = true
let studySampleNameColumn = true
let invesXlsx = {Path = "bla"; Cell = "B17"}
let studyRegisteredInInves = true
let studyFactor = true
let assayXlsx = {Path = "bla"; Cell = "B2"}
let termsAvailable = true

isa [
    schema [
        Sublevel2.study [
            sourceNameColumn (fun () -> isPresent studySourceNameColumn (XlsxFileMessage studyXlsx))
            sampleNameColumn (fun () -> isPresent studySampleNameColumn (XlsxFileMessage studyXlsx))
        ]
        assay (fun () -> isRegistered studyRegisteredInInves (XlsxFileMessage invesXlsx))
    ]
    semantic [
        Sublevel2.assay [
            term (fun () -> isValidTerm termsAvailable (XlsxFileMessage assayXlsx))
        ]
    ]
    plausibility [
        Sublevel2.study [
            factor (fun () -> isPresent studyFactor (XlsxFileMessage studyXlsx))
        ]
    ]
]

let investigationPath = "bla"
let investigationPresence = System.IO.File.Exists investigationPath
let assaysPath = "bla"
let assaysPresence = System.IO.File.Exists investigationPath

filesystem [
    investigation (fun () -> isPresent investigationPresence (FileSystemEntityMessage investigationPath))
    assays (fun () -> isPresent assaysPresence (FileSystemEntityMessage assaysPath))
]