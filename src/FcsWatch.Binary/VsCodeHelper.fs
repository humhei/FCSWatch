namespace FcsWatch.Binary
open Fake.IO
open FcsWatch.Core


module internal VscodeHelper =

    type Configuration =
        { name: string
          ``type``: string
          request: string
          preLaunchTask: obj
          program: obj
          args: obj
          processId: obj
          justMyCode: obj
          cwd: obj
          stopAtEntry: obj
          console: obj }

    type Launch =
        { version: string
          configurations: Configuration list }

    [<RequireQualifiedAccess>]
    module Launch =
        open Newtonsoft.Json

        let read file =

            let jsonTest = File.readAsString file
            JsonConvert.DeserializeObject<Launch> jsonTest

        let write file (launch: Launch) =
            let settings = JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore)
            let jsonText = JsonConvert.SerializeObject(launch,Formatting.Indented,settings)
            File.writeString false file jsonText

        let writePid configurationName pid (launch: Launch) =
            { launch with
                configurations =
                    let prediate configuration = configuration.request = "attach" && configuration.name = configurationName

                    launch.configurations
                    |> List.tryFind prediate
                    |> function
                        | Some _ -> ()
                        | None ->
                            logger.Info "Cannot find attachable configuration named %s in lanuch.json" configurationName

                    let newConfigurations = 

                        launch.configurations
                        |> List.map (fun configuration ->
                            if prediate configuration then
                                {configuration with processId = pid}
                            else 
                                configuration
                        ) 

                    newConfigurations
            }
