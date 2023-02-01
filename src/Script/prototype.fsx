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


let filesystem =
    testList "Filesystem" [
        
    ]

let isaTests =
    testList "ISA" [
        testCase "Schema" <| fun () ->
            
    ]