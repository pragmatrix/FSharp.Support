module LivePipes.FSharp.ProjectOptions

open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

/// Helper for FSharpProjectOptions
type private Options = FSharpProjectOptions

[<Literal>]
let OutputFilePrefix = "--out:"
[<Literal>]
let AssemblyReferencePrefix = "-r:"
         
let outputDirectory (options: Options) = 
    options.OtherOptions
    |> Seq.find (fun str -> str.StartsWith(OutputFilePrefix))
    |> fun str -> str.Substring(OutputFilePrefix.Length)
    |> Path.GetDirectoryName

let assemblyReferences (options: Options) =
    options.OtherOptions
    |> Seq.filter (fun str -> str.StartsWith(AssemblyReferencePrefix))
    |> Seq.map (fun str -> str.Substring(AssemblyReferencePrefix.Length))
    |> Seq.toList

let projectFileNames options = 
    options.OtherOptions
    |> Seq.filter (fun str -> not <| str.StartsWith("-") && str.ToLowerInvariant().EndsWith(".fs"))
    |> Seq.toList
