module ValidationResultApi

open Giraffe
open System.IO

// KF: header im Post-Request-JSON hat URL vom Absender und kann somit benutzt werden, um 

module Handler =

    let checkResult : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) -> 
            // modified from: https://stackoverflow.com/questions/17232414/creating-a-zip-archive-in-memory-using-system-io-compression
            task {
                let testResult = ctx.Request.Body
                let ms = new MemoryStream()
                let! _ = testResult.CopyToAsync(ms)
                let testResultBA = ms.ToArray()

                let getByteArray (fileName : string) (data : byte []) =
                    try
                        use ms = new MemoryStream()
                        (
                            use archive = new ZipArchive(ms, ZipArchiveMode.Create)
                            let entry = archive.CreateEntry(fileName)
                            use entryStream = entry.Open()
                            use bw = new BinaryWriter(entryStream)
                            bw.Write(data)
                        )
                        (ms.ToArray())
                    with e -> failwithf "Cannot zip stream %s: %s" fileName e.Message
                let res = getByteArray "arc.json" isaJsonBA
                return! ctx.WriteBytesAsync res
            }

module Docs =

    let view : HttpHandler = 
        fun (next : HttpFunc) (ctx : HttpContext) -> 