module FsLive.Core.DebuggingServer 
open Fake.IO
open System.Net
open Fake.IO.FileSystemOperators
open FsLive.Core.CompilerTmpEmiiter
open System.Text
open CrackedFsprojBundle
open Suave.Filters
open Suave.Operators
open Suave
open System.Net.Sockets


let rec freePort() =
    let listener = new TcpListener(IPAddress.Any, 0);
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    listener.Stop()
    port

let generateCurlCache freePort (config: Config) =         
    let cacheDir = config.WorkingDir </> ".fake" </> "fcswatch"

    let fileName = cacheDir </> "port.cache"

    Directory.ensure cacheDir

    let lines = 
        [ sprintf "url: http://localhost:%d/emitCompilerTmp" freePort
          "-f" ]

    File.writeWithEncoding Encoding.ASCII false fileName lines

[<RequireQualifiedAccess>]
type DebuggingServerMsg =
    | EmitCompilerTmp of replyChannel: AsyncReplyChannel<WebPart>

let debuggingServer (compilerTmpEmitterAgent: MailboxProcessor<CompilerTmpEmitterMsg>) config = MailboxProcessor<DebuggingServerMsg>.Start(fun inbox ->
    async {
        let webApp =
            let emitCompilerTmp: WebPart = 
                fun (ctx : HttpContext) ->
                    async {
                        let! handler = 
                            inbox.PostAndAsyncReply(fun replyChannel ->
                                DebuggingServerMsg.EmitCompilerTmp replyChannel
                            )
                        return! handler ctx  
                    }

            choose [
                path "/emitCompilerTmp" >=> emitCompilerTmp
            ]
        let config =

            let freePort = freePort()
            generateCurlCache freePort config
            let local = Suave.Http.HttpBinding.createSimple HTTP "127.0.0.1" freePort

            { defaultConfig with bindings = [local] }

        startWebServer config webApp
    } |> Async.Start

    let rec loop state = async {
        let! msg = inbox.Receive()
        match msg with 
        | DebuggingServerMsg.EmitCompilerTmp replyChannel ->
            async {
                let msg = compilerTmpEmitterAgent.PostAndReply CompilerTmpEmitterMsg.Emit          
                replyChannel.Reply msg
            } |> Async.Start
            return! loop state  
    }
    loop ()
) 
