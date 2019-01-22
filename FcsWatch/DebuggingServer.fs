module FcsWatch.DebuggingServer 
open Fake.IO
open Saturn
open System.Net
open Fake.IO.FileSystemOperators
open FcsWatch.CompilerTmpEmiiter
open Giraffe.Core
open Giraffe.Routing
open FSharp.Control.Tasks.V2.ContextSensitive
open Microsoft.AspNetCore.Http
open Giraffe.ResponseWriters
open FcsWatch.Types
open System
open Microsoft.Extensions.Logging
open Giraffe.HttpStatusCodeHandlers
open System.Text
let freePort() =
    let l = Sockets.TcpListener(System.Net.IPAddress.Loopback, 0)
    l.Start()
    let port = (l.LocalEndpoint :?> IPEndPoint).Port
    l.Stop()
    port

let generateCurlCache freePort config =         
    let cacheDir = config.WorkingDir </> ".fake" </> "fcswatch"
    let fileName = cacheDir </> "port.cache"
    Directory.ensure cacheDir
    let lines = 
        [
            sprintf "url: http://localhost:%d/emitCompilerTmp" freePort
            "-f"           
        ]
    File.writeWithEncoding Encoding.ASCII false fileName lines

[<RequireQualifiedAccess>]
type DebuggingServerMsg =
    | EmitCompilerTmp of replyChannel: AsyncReplyChannel<HttpHandler>
    | StartServer

let debuggingServer (compilerTmpEmitterAgent: MailboxProcessor<CompilerTmpEmitterMsg>) config = MailboxProcessor<DebuggingServerMsg>.Start(fun inbox ->
    
    let webApp =
        choose [
            route "/emitCompilerTmp"   >=>  
                fun (next : HttpFunc) (ctx : HttpContext) ->
                    task {
                        let! handler = 
                            inbox.PostAndAsyncReply(fun replyChannel ->
                                DebuggingServerMsg.EmitCompilerTmp replyChannel
                            )
                        return! handler next ctx  
                    }
                 
        ]
    let freePort = freePort()
    generateCurlCache freePort config

    let app =
        application {
            url (sprintf "http://localhost:%d" freePort) 
            use_router webApp
        }

    let rec loop state = async {
        let! msg = inbox.Receive()
        match msg with 
        | DebuggingServerMsg.EmitCompilerTmp replyChannel ->
            async {
                let msg = compilerTmpEmitterAgent.PostAndReply CompilerTmpEmitterMsg.Emit          
                replyChannel.Reply msg
            } |> Async.Start
            return! loop state  
        | DebuggingServerMsg.StartServer -> 
            async {run app} |> Async.Start
            return! loop state
    }
    loop ()
) 
