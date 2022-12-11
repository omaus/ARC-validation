#load "checkArcStructure.fsx"
#load "checkIsaStructure.fsx"
#load "summaryFileWriter.fsx"
#r "nuget: Expecto"
#r "nuget: FSharpAux"

open CheckArcStructure
open CheckIsaStructure
open SummaryFileWriter
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

writeNUnitSummary fp res

// XSD format makes autogenerating Readers easy. XSD für NUnit XML v2: https://nunit.org/files/testresult_schema_25.txt