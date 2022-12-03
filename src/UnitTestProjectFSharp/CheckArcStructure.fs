namespace ARCValidation

open System.IO

module CheckArcStructure =

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

    /// Checks if `path` contains a .arc folder.
    let checkForArcFolder path = Directory.Exists (Path.Combine(path, ".arc"))

    /// Checks if `path` contains a .git folder and all of its basic files.
    let checkForGitFolderStructure path =
        let gitPath = Path.Combine(path, ".git")
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

    /// Checks if `path` contains a Studies folder.
    let checkForStudiesFolder path = Directory.Exists (Path.Combine(path, "studies"))

    /// Checks if `path` contains an Assays folder.
    let checkForAssaysFolder path = Directory.Exists (Path.Combine(path, "assays"))

    /// Checks if `path` contains a Runs folder.
    let checkForRunsFolder path = Directory.Exists (Path.Combine(path, "runs"))

    /// Checks if `path` contains a Workflows folder.
    let checkForWorkflowsFolder path = Directory.Exists (Path.Combine(path, "workflows"))

    /// Checks if `path` contains an Investigation file.
    let checkForInvestigationFile path = File.Exists (Path.Combine(path, "isa.investigation.xlsx"))

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