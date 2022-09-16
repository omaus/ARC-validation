open System.IO

// Types:

/// Type representation of a Study folder.
type StudyFolderStructure = {
    Name                : string
    Path                : string
    HasIsaFile          : bool
    HasResourcesFolder  : bool
}

/// Creates a StudyFolderStructure from given parameters.
let createStudyFolderStructure name path hasIsaFile hasResourcesFolder hasProtocolsFolder = {
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

/// Creates a StudyFolderStructure from given parameters.
let createAssayFolderStructure name path hasIsaFile hasDatasetFolder = {
    Name                = name
    Path                = path
    HasIsaFile          = hasIsaFile
    HasDatasetFolder    = hasDatasetFolder
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

/// Takes a possible Elements folder path (`string option`) and returns the possible collection of all Elements folders' paths in it. This applies to both Studies and Assays as Elements.
let getElementInElementsFolder elementsFolder =
    match elementsFolder with
    | None -> None
    | Some ef -> Directory.GetDirectories ef |> Some

/// Takes a possible collection of Studies' paths (`string [] option`) and returns the possible folder structure of each Study.
let checkStudiesFolderStructure studiesInStudiesFolder =
    match studiesInStudiesFolder with
    | None -> None
    | Some studies ->
        studies
        |> Array.map (
            fun dir ->
                let n = (DirectoryInfo dir).Name
                let isaf = Path.Combine(dir, "isa.study.xlsx") |> File.Exists
                let rf = Path.Combine(dir, "resources") |> Directory.Exists
                createStudyFolderStructure n dir isaf rf
        )
        |> Some

/// Takes a possible collection of Assays' paths (`string [] option`) and returns the possible folder structure of each Assay.
let checkAssaysFolderStructure assaysInAssaysFolder =
    match assaysInAssaysFolder with
    | None -> None
    | Some assays ->
        assays
        |> Array.map (
            fun dir ->
                let n = (DirectoryInfo dir).Name
                let isaf = Path.Combine(dir, "isa.assay.xlsx") |> File.Exists
                let df = Path.Combine(dir, "dataset") |> Directory.Exists
                createAssayFolderStructure n dir isaf df
        )
        |> Some