#r "nuget: Expecto"
#r "nuget: FSharpAux"

open Expecto
open Impl
open FSharpAux
open System
open System.IO
open System.Xml
open System.Xml.Linq
open System.Globalization
open System.Reflection

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

// TO DO: Adjust JUnit writer from haf/expecto

// XSD format makes autogenerating Readers easy. XSD f�r NUnit XML v2: https://nunit.org/files/testresult_schema_25.txt