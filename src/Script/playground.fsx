#load "checkArcStructure.fsx"
#load "checkIsaStructure.fsx"
#r "nuget: Expecto"
#r "nuget: FSharpAux"

open CheckArcStructure
open CheckIsaStructure
open Expecto
open Expecto.Expect
open ISADotNet.XLSX
open System.IO
open FSharpAux


let isRegistered actual message =
  if actual then ()
  else
    failtestf "%s. Actual value was not registered." message     // <- string hier ist expliziter Fehler (ohne Ort, Ort wird über message realisiert), also Fehlermeldung zum Name der Funktion

// as path (tree) -> Arc Structure, (isa) content
// let isParameterConsistent actual tree message =
// isParameterConsistent myStudy.Parameters tree "hier wäre dann der Baum"
// isParameterConsistent myAssay.Parameters tree "hier wäre dann der Baum"
// message = genaue Position, geht aus dem Objekt hervor (dynamisch); 


// let pathToCheck = System.Environment.GetCommandLineArgs() |> Array.last
let pathToCheck = @"C:\Users\revil\OneDrive\CSB-Stuff\NFDI\testARC26"

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
    | Some ssfs -> ssfs |> Array.map (fun ssf -> StudyFile.Study.fromFile (Path.Combine(ssf, "isa.study.xlsx"))) |> List.ofArray
    | None      -> []
let assays =
    match assaysInAssaysFolder with
    | Some aafs -> aafs |> Array.map (fun aaf -> AssayFile.Assay.fromFile (Path.Combine(aaf, "isa.assay.xlsx")) |> snd) |> List.ofArray
    | None      -> []

let studyRegistration   = checkStudiesRegistration studies investigation
let assayRegistration   = checkAssaysRegistration assays investigation

let fileStructureTests = testList "ARC filesystem structure tests" [
    testList "Folder structure tests" [
        testCase "Has .arc folder"              <| fun () -> isRegistered hasArcFolder            ".arc folder does not exist"
        testCase "Has Git folder structure"     <| fun () -> isTrue hasGitFolderStructure   "Git folder structure does not exist"
        testCase "Has Studies folder"           <| fun () -> isTrue hasStudiesFolder        "Studies folder does not exist"
        testCase "Has Assays folder"            <| fun () -> isTrue hasAssaysFolder         "Assays folder does not exist"
    ]
    testList "File structure tests" [
        testCase "Has ISA Investigation file"   <| fun () -> isTrue hasInvestigationFile    "ISA Investigation file does not exist"
    ]
]

// Task für BioHackathon: 
//      1. dann den Writer schreiben, 
//      2. Für alle dieser testListen einen Case machen (also 1 für Schema etc.), 
//      3. dann ein Bsp. XML File daraus generieren


// rechte Seite: möglichst wenig Fälle: Am besten sowas wie: nur Pfad, Worksheet & Zelle bzw. Pfad, Line & Pos
// links: dann so ausgleichen, dass es zusammen mit rechter Seite + statische Funktionsmessage vollkommen Sinn ergibt

let isaStructureTests = testList "ISA" [
    testList "Schema" [     // kann man alles mit JSON Schema testen, auch ISA (ist das ISA-Format korrekt?); bei Bedarf mit Content fusionieren
    ]
    testList "Content" [    // sind die Content-Regeln eingehalten?
        testCase "Study"   <| fun () -> isRegistered studyRegistration.AreStudiesRegistered $"ID: {studyRegistration.}, Worksheet: {z}, Cell: {y}"
        testCase "Assay"    <| fun () -> isRegistered assayRegistration.AreAssaysRegistered "Not all assays are registered"
    ]
    testList "Semantic" [   // z. B. haben alle Terme Identifier? Ist es CWL-complient?
    ]
    testList "Plausibility" [  // z. B. gibt es überhaupt einen Faktor? Macht das ISA Objekt wissenschaftlich Sinn?
        testCase "Replicate"     <| fun () -> isEqual x ""
    ]
]

let arcValidationTest = testList "ARC validation" [fileStructureTests; isaStructureTests]

Tests.runTestsWithCLIArgs [] [|"--nunit-summary"; "TestResults.xml"|] arcValidationTest
//Tests.runTestsWithCLIArgs [] [|"--junit-summary"; "TestResults.junit.xml"|] arcValidationTest

#r "nuget: FsCheck"
#r "nuget: Expecto.FsCheck"
open FsCheck
FsCheckConfig.defaultConfig
let tp = Expecto.FsCheck.Property("lol", fun x -> x)    // k�nnte man sich �berlegen, hier weiter herumzuspielen
Tests.runTestsWithCLIArgs [||] [|"--nunit-summary"; "TestResultsTp.xml"|] tp


Tests.defaultConfig.verbosity
let tcb = Expecto.Tests.test "lol"
let testtest = tcb.Run (fun () -> isTrue ("lal" = "lol") "not true")
Tests.runTestsWithCLIArgs [||] [||] testtest

let logger = Expecto.Logging.Log.create "myLogger"
//logger.

Expecto.Impl.evalTests Tests.defaultConfig arcValidationTest
|> Async.RunSynchronously
|> List.map (fun (ft,ts) -> ft.name)



Expecto.Tests.runTestsWithCancel

//let studiesFromInves = 
//    match investigation.Studies with
//    | Some sfis -> sfis
//    | None      -> []
//let assaysFromStudies = 
//    studiesFromInves
//    |> List.collect (
//        fun sfi -> 
//            match sfi.Assays with
//            | Some ass -> ass
//            | None -> []
//    )

//let assaysOutersect = List.outersect assaysFromStudies assays

//assayRegistration.AreAssaysRegistered
//assayRegistration.MissingAssays
//assayRegistration.UnregisteredAssays

let studyFromFile = StudyFile.Study.fromFile @"C:\Users\olive\OneDrive\CSB-Stuff\NFDI\testARC26\studies\sid1\isa.study.xlsx"
let studyFromInvestigation = investigation.Studies.Value[0]
studyFromFile.Protocols.Value

#r "nuget: NUnit"

open NUnit.Framework
open System

[<TextFixture>]
type TestClass() =
    [<Test>]
    member this.Sheesh() = Assert.True true


// Slightly modified from https://github.com/haf/expecto/blob/main/Expecto/TestResults.fs
/// Functions for writing test summaries.
module SummaryWriter =
    
    open Impl
    open System.Xml
    open System.Xml.Linq
    open System.Globalization
    open System.Reflection

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

    /// Generate test results using NUnit v2 schema.
    let writeNUnitSummary file (summary : TestRunSummary) =
        // v3: https://github.com/nunit/docs/wiki/Test-Result-XML-Format
        // this impl is v2: http://nunit.org/docs/files/TestResult.xml
        let totalTests = summary.errored @ summary.failed @ summary.ignored @ summary.passed
        let testCaseElements =
            totalTests
            |> Seq.sortByDescending (fun (_,test) -> test.result.order, test.duration.TotalSeconds)
            |> Seq.map (fun (flatTest, test) ->
                    
                let fullnameString = 
                    flatTest.name 
                    |> List.fold (fun acc s -> acc+ s + ";" ) "[ "
                    |> fun s -> s[.. String.length s - 2] + " ]"
                let element = XElement(XName.Get "test-case", XAttribute(XName.Get "name", fullnameString))
                let addAttribute name (content : string) = element.Add(XAttribute(XName.Get name, content))

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
                    failureNode.Add(XName.Get "stack-trace", XCData e.StackTrace)
                    element.Add failureNode
                | Failed msg ->
                    failureNode.Add(XName.Get "message", XCData msg)
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
        element
        |> XDocument
        |> xmlSave file


let dummyTests = testList "node1/top" [
    testList "node2" [
        testList "node 3" [
            testList "node 4" [
                testCase "node5/case" <| fun () -> isTrue true  "was true"
                testCase "node5/case" <| fun () -> isTrue false "was false"
            ]
        ]
    ]
]

open Expecto.Impl

let res =
    let w = Diagnostics.Stopwatch()
    w.Start()
    Expecto.Impl.evalTests Tests.defaultConfig dummyTests
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

//let fp = @"C:\Users\olive\OneDrive\CSB-Stuff\NFDI\testFolder/testresult.xml"
let fp = @"C:\Users\revil\OneDrive\CSB-Stuff\NFDI\testFolder/testresult.xml"

SummaryWriter.writeNUnitSummary fp res

// XSD format makes autogenerating Readers easy. XSD für NUnit XML v2: https://nunit.org/files/testresult_schema_25.txt