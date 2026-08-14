/// The language server's one shared, reloadable static-analysis graph -
/// loaded once at process start via `init`, and reloadable in place via
/// `reload` (the `moodev/reloadGraph` custom method in `Handlers.fs` calls
/// this) without restarting the process. `Program.fs` passes the `get`
/// accessor itself (not a called snapshot) through `WsTransport.run` into
/// `MooLspServer`, which calls `getGraph ()` fresh at the top of every
/// graph-dependent method - the browser's `/lsp` connection is held open for
/// its entire page session, so a snapshot taken once per connection (the
/// original design here) would never observe a later reload at all. Every
/// existing `Handlers.fs` function below `MooLspServer` still just takes a
/// plain immutable `Graph` parameter, unchanged - only the server class
/// itself re-fetches per request.
module LanguageServer.GraphStore

let mutable private current: Metadata.Schema.Graph =
    { Objects = Map.empty
      SystemObjectProperties = Map.empty
      Builtins = Map.empty }

let init (surviveRoot: string) : unit = current <- Metadata.Loader.load surviveRoot

let reload (surviveRoot: string) : unit =
    printfn "Reloading metadata graph from %s..." surviveRoot
    current <- Metadata.Loader.load surviveRoot
    printfn "Loaded %d objects." current.Objects.Count

let get () : Metadata.Schema.Graph = current
