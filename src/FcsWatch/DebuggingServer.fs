module FcsWatch.DebuggingServer 
open Fake.IO
open System.Net
open Fake.IO.FileSystemOperators
open FcsWatch.CompilerTmpEmiiter
open System.Text
open Types
open Suave.Filters
open Suave.Operators
open Suave
open System.Net.NetworkInformation

let mutable allPorts = 
    let ipProperties = IPGlobalProperties.GetIPGlobalProperties()
    ipProperties.GetActiveTcpListeners() 
        |> List.ofArray 
        |> List.map (fun endpoint -> endpoint.Port)

let rec freePort() =
    let newPort = System.Random().Next(65535)
    match List.contains newPort allPorts with 
    | true  -> freePort()
    | false ->
        allPorts <- newPort :: allPorts
        newPort

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
