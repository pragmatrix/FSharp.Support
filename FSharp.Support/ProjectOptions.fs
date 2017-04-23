namespace FSharp.Support

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

type ProjectOptions = {
    ProjectFile: string
    OutputFile: string
    SourceFiles: string list
    AssemblyReferences: string list
    Options: string list
} with
    member this.OutputDirectory =
        this.OutputFile |> Path.GetDirectoryName
        

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ProjectOptions =

    let empty = {
        ProjectFile = ""
        OutputFile = ""
        SourceFiles = []
        AssemblyReferences = []
        Options = []
    }

    let (|Prefixed|_|) (prefix: string) (input: string) =
        if input.Length >= prefix.Length && input.[0..prefix.Length-1] = prefix then
            Some (input.[prefix.Length..])
        else
            None

    [<Literal>]
    let OutputFilePrefix = "--out:"
    let AssemblyReferencePrefix = "-r:"

    let fromFSharpOptions (options: FSharpProjectOptions) : ProjectOptions = 
        let processOption option options = 
            match option with
            | Prefixed OutputFilePrefix outputFile ->
                { options with OutputFile = outputFile }
            | Prefixed AssemblyReferencePrefix reference ->
                { options with AssemblyReferences = reference :: options.AssemblyReferences}
            | sourceFile when not (option.StartsWith("-")) && (option.ToLowerInvariant().EndsWith(".fs")) ->
                { options with SourceFiles = sourceFile :: options.SourceFiles }
            | option ->
                { options with Options = option :: options.Options }
    
        Array.foldBack processOption options.OtherOptions empty
        |> fun o -> { o with ProjectFile = options.ProjectFileName }

    let toFSharpOptions (options: ProjectOptions) : FSharpProjectOptions = 
        
        let otherOptions =
            [
                yield! options.SourceFiles
                yield! options.AssemblyReferences |> List.map (fun str -> AssemblyReferencePrefix + str)
                yield OutputFilePrefix + options.OutputFile
                yield! options.Options
            ]

        {
            ProjectFileName = options.ProjectFile
            ProjectFileNames = [||]
            OtherOptions = otherOptions |> List.toArray
            ReferencedProjects = [||]
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = false
            LoadTime = DateTime.Now
            UnresolvedReferences = None
            OriginalLoadReferences = []
            ExtraProjectInfo = None
        }
        