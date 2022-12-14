#load "checkArcStructure.fsx"
#load "checkIsaStructure.fsx"
#load "summaryFileWriter.fsx"
#load "evaluation.fsx"
#r "nuget: Expecto"
#r "nuget: FSharpAux"

open CheckArcStructure
open CheckIsaStructure
open SummaryFileWriter
open Evaluation
open Expecto
open Expecto.Expect
open ISADotNet.XLSX
open System.IO
open FSharpAux
open Build
open Build.Toplevel
open Build.Sublevel1
open Build.Sublevel2
open Build.CaseLevel
open Check


let studyXlsx = {Path = @"C:\Users\Admin\testARC\studies\study1\study1.isa.xlsx"; Cell = "AC2"}
let studySourceNameColumn = true
let studySampleNameColumn = true
let invesXlsx = {Path = "bla"; Cell = "B17"}
let studyRegisteredInInves = true
let studyFactor = true
let assayXlsx = {Path = @"C:\Users\Admin\testARC\assays\assay1\assay1.isa.xlsx"; Cell = "B2"}
let termsAvailable1 = true
let termsAvailable2 = false

let isaTests =
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
                term (fun () -> isValidTerm termsAvailable1 (XlsxFileMessage assayXlsx))
                term (fun () -> isValidTerm termsAvailable2 (XlsxFileMessage assayXlsx))
            ]
        ]
        plausibility [
            Sublevel2.study [
                factor (fun () -> isPresent studyFactor (XlsxFileMessage studyXlsx))
            ]
        ]
    ]

let res = performTest isaTests
let fp = Some @"C:\Users\olive\OneDrive\CSB-Stuff\NFDI\testFolder/testresult.xml"
writeNUnitSummary fp res
writeNUnitSummary None res

let investigationPath = "bla"
let investigationPresence = System.IO.File.Exists investigationPath
let assaysPath = "bla"
let assaysPresence = System.IO.File.Exists investigationPath

filesystem [
    CaseLevel.investigation (fun () -> isPresent investigationPresence (FileSystemEntityMessage investigationPath))
    CaseLevel.assays (fun () -> isPresent assaysPresence (FileSystemEntityMessage assaysPath))
]

#r "nuget: ISADotNet"
#r "nuget: ISADotNet.XLSX"
#r "nuget: ISADotNet.Validation"

open ISADotNet
open ISADotNet.XLSX
open ISADotNet.Validation

let testAssayPath = @"C:\Users\olive\OneDrive\CSB-Stuff\NFDI\testARC26\assays\aid1\isa.assay.xlsx"
let _, testAssay = AssayFile.Assay.fromFile testAssayPath
let testAssayJson = Json.Assay.toString testAssay

JSchema.validateAssay testAssayJson
JSchema.validateFactor testAssayJson

let testInvesPath = @"C:\Users\olive\OneDrive\CSB-Stuff\NFDI\testARC26\isa.investigation.xlsx"
let testInves = Investigation.fromFile testInvesPath
let testInvesJson = Json.Investigation.toString testInves

JSchema.validateInvestigation testInvesJson

// Tests:
// - Missing StudyIdentifier despite other Study information is simply ignored and returns Okay validation wtf

let testStudyPath = @"C:\Users\olive\OneDrive\CSB-Stuff\NFDI\testARC26\studies\sid1\isa.study.xlsx"
let testStudy = StudyFile.Study.fromFile testStudyPath
let testStudyJson = Json.Study.toString testStudy

JSchema.validateStudy testStudyJson