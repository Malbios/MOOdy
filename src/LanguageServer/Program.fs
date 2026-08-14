module LanguageServer.Program

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    let app = builder.Build()

    let surviveRoot = app.Configuration.GetValue<string>("Survive:Root", "../../../Survive")
    printfn "Loading metadata graph from %s..." surviveRoot
    GraphStore.init surviveRoot
    printfn "Loaded %d objects." (GraphStore.get ()).Objects.Count

    // The Sidecar's own `/lsp-bridge` endpoint (see `Sidecar/LspBridge.fs`),
    // never the browser-facing `/ws` - a single shared bridge for every
    // `/lsp` connection this process accepts (normally just one, the
    // browser's own Monaco session), lazily (re)connected on first live
    // query rather than blocking startup on the Sidecar being up yet.
    let sidecarBridgeUrl = app.Configuration.GetValue<string>("Sidecar:BridgeUrl", "ws://127.0.0.1:5000/lsp-bridge")
    let bridge = SidecarBridge.create sidecarBridgeUrl

    app.UseWebSockets() |> ignore

    app.Map(
        "/lsp",
        Func<HttpContext, Task>(fun ctx ->
            task {
                if ctx.WebSockets.IsWebSocketRequest then
                    use! webSocket = ctx.WebSockets.AcceptWebSocketAsync()
                    // `GraphStore.get` (the accessor function, not a called
                    // snapshot) is passed straight through so every request
                    // on this connection - not just the first one - reads
                    // whatever's current at request time. The browser holds
                    // one `/lsp` connection open for its whole page session,
                    // so a snapshot taken here at accept time would never see
                    // a later `moodev/reloadGraph` at all. `Server.startWs`
                    // blocks for the connection's lifetime running its own
                    // message loop - off the async continuation thread via
                    // Task.Run so it doesn't tie up a thread-pool thread for
                    // the whole session.
                    do! Task.Run(fun () -> WsTransport.run webSocket GraphStore.get bridge)
                else
                    ctx.Response.StatusCode <- StatusCodes.Status400BadRequest
            })
    )
    |> ignore

    app.Run()

    0
