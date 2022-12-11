#r "nuget: Expecto"

open Expecto

module Build =

    // toplevel
    let isa = testList "ISA"
    let filesystem = testList "Filesystem"

    // first sublevel
    let schema = testList "Schema"    // alt.: let content = testList "Content"

    // case level
    let study = testCase "Study"
    let assay = testCase "Assay"

module Check =

    let isPresent actual message = 
        if actual then ()
        else failtestf "%s. Actual file/folder is not present." message     // <- string hier ist expliziter Fehler (ohne Ort, Ort wird über message realisiert), also Fehlermeldung zum Name der Funktion

    let isRegistered actual message =
      if actual then ()
      else
        failtestf "%s. Actual value was not registered." message     // <- string hier ist expliziter Fehler (ohne Ort, Ort wird über message realisiert), also Fehlermeldung zum Name der Funktion

open Build

isa [
    schema [case1]
]