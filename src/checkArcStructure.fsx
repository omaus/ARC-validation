open System.IO

// Types:

/// Type representation of a Study folder.
type StudyFolderStructure = {
    Name                : string
    Path                : string
    HasIsaFile          : bool
    HasResourcesFolder  : bool
    HasProtocolsFolder  : bool
}

/// Creates a StudyFolderStructure from given parameters.
let createStudyFolderStructure name path hasIsaFile hasResourcesFolder hasProtocolsFolder = {
    Name                = name
    Path                = path
    HasIsaFile          = hasIsaFile
    HasResourcesFolder  = hasResourcesFolder
    HasProtocolsFolder  = hasProtocolsFolder
}

/// Type representation of an Assay folder.
type AssayFolderStructure = {
    Name                : string
    Path                : string
    HasIsaFile          : bool
    HasDatasetFolder    : bool
    HasProtocolsFolder  : bool
}

/// Creates a StudyFolderStructure from given parameters.
let createAssayFolderStructure name path hasIsaFile hasDatasetFolder hasProtocolsFolder = {
    Name                = name
    Path                = path
    HasIsaFile          = hasIsaFile
    HasDatasetFolder    = hasDatasetFolder
    HasProtocolsFolder  = hasProtocolsFolder
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