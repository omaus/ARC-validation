module ValidationResultApi

// KF: header im Post-Request-JSON hat URL vom Absender und kann somit benutzt werden, um 

module Handler =

    let checkResult : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) -> 

module Docs =

    let view : HttpHandler = 
        fun (next : HttpFunc) (ctx : HttpContext) -> 