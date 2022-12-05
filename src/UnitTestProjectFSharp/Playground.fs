module PlaygroundExpecto

open ARCValidation
open CheckArcStructure
open CheckIsaStructure
open Expecto
open Expecto.Expect
open ISADotNet.XLSX
open System.IO
open NUnit

module PGExpecto =
    
    let pathToCheck = System.Environment.GetCommandLineArgs() |> Array.last
    // let pathToCheck = @"C:\Users\olive\OneDrive\CSB-Stuff\NFDI\testARC26"

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
            testCase "All studies are registered"   <| fun () -> isTrue studyRegistration.AreStudiesRegistered "Not all studies are registered"
        ]
        testList "ISA Assay tests" [
            testCase "All assays are registered"    <| fun () -> isTrue assayRegistration.AreAssaysRegistered "Not all assays are registered"
        ]
    ]

    let arcValidationTest = testList "ARC validation" [fileStructureTests; isaStructureTests]

    Tests.runTestsWithCLIArgs [] [|"--nunit-summary"; "TestResults.xml"|] arcValidationTest
    //Tests.runTestsWithCLIArgs [] [|"--junit-summary"; "TestResults.junit.xml"|] arcValidationTest

    open FsCheck
    FsCheckConfig.defaultConfig
    let tp = Expecto.FsCheck.Property("lol", fun x -> x)    // könnte man sich überlegen, hier weiter herumzuspielen
    Tests.runTestsWithCLIArgs [||] [|"--nunit-summary"; "TestResultsTp.xml"|] tp


    Tests.defaultConfig.verbosity
    let tcb = Expecto.Tests.test "lol"
    let testtest = tcb.Run (fun () -> isTrue ("lal" = "lol") "not true")
    Tests.runTestsWithCLIArgs [||] [||] testtest

    let logger = Expecto.Logging.Log.create "myLogger"
    //logger.


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

module PGNUnit =

    asd
    NUnit
    Expecto
    let x = 7