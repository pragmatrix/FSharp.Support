module SignatureTests

open System
open System.IO
open FsUnit
open Xunit
open Microsoft.FSharp.Compiler.SourceCodeServices
open LivePipes.FSharp

let checker = FSharpChecker.Create(keepAssemblyContents = true)

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

type CompareCommand =
    | Equal
    | NotEqual

type CompareInst = CompareInst of CompareCommand * string * string

let makeCommand (str: string) = 
    let equal = str.Split([|"// ==\r\n"|], StringSplitOptions.None)
    match equal with
    | [|a;b|] -> (Equal, a, b) |> CompareInst
    | _ ->
    let notEqual = str.Split([|"// !=\r\n"|], StringSplitOptions.None)
    match notEqual with
    | [|a;b|] -> (NotEqual, a, b) |> CompareInst
    | _ -> failwithf "failed to find // == or // != in %A" str  

let compare (CompareInst(cmd, a, b)) = 
    let accessibility = Signature.Public

    let getSigFromFile file =
        let projectResults = parseAndCheckSingleFile file
        if projectResults.Errors.Length <> 0 then
            failwithf "found errors or warningn: %A" projectResults.Errors
        let implementationFiles = projectResults.AssemblyContents.ImplementationFiles
        implementationFiles.Length |> should equal 1
        Signature.create accessibility implementationFiles.[0]

    let sigA = a |> getSigFromFile
    let sigB = b |> getSigFromFile
    printfn "%s" <| sprintf "sigA: %A" sigA
    printfn "sigB: %A" sigB
    match cmd with
    | Equal -> sigA |> should equal sigB
    | NotEqual -> sigA |> should not' (equal sigB)

[<Fact>]
let testAll() =
    let testFile = File.ReadAllText("SignatureTestFilesPublic.fs_")
    let compareInstructions = 
        testFile.Split([|"// --\r\n"|], StringSplitOptions.None)
        |> Seq.map makeCommand
        |> Seq.toList

    compareInstructions
    |> List.iter compare
    

