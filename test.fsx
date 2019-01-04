// Learn more about F# at http://fsharp.org

open System
open System.Collections.Concurrent
open System.Threading
open System.IO
open System.Net

let freePort =
    let l = Sockets.TcpListener(System.Net.IPAddress.Loopback, 0)
    l.Start()
    let port = (l.LocalEndpoint :?> IPEndPoint).Port
    l.Stop()
    port