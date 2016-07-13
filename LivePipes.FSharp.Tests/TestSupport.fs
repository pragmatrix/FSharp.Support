[<AutoOpen>]
module TestSupport

open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

let checker = FSharpChecker.Create(keepAssemblyContents = false)

let projectOptions = 
    let file = Path.GetFullPath("Main.fsx")

    File.WriteAllText(file, "")
    // Get context representing a stand-alone (script) file
    checker.GetProjectOptionsFromScript(file, "Main.fsx")
    |> Async.RunSynchronously

let parseAndCheckSingleFile input = 
    let po = projectOptions
    File.WriteAllText("Main.fsx", input)
    checker.ParseAndCheckProject(po) 
    |> Async.RunSynchronously
