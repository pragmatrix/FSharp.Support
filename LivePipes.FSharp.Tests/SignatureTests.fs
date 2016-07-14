module SignatureTests

open System
open System.IO
open FsUnit
open Xunit
open LivePipes.FSharp

type CompareCommand =
    | Equal
    | NotEqual

type CompareInst = CompareInst of CompareCommand * string * string

let makeCommand (str: string) = 
    let equal = str.Split([|"// ==\n"|], StringSplitOptions.None)
    match equal with
    | [|a;b|] -> (Equal, a, b) |> CompareInst
    | _ ->
    let notEqual = str.Split([|"// !=\n"|], StringSplitOptions.None)
    match notEqual with
    | [|a;b|] -> (NotEqual, a, b) |> CompareInst
    | _ -> failwithf "failed to find // == or // != in %A" str  

let compare (CompareInst(cmd, a, b) as inst) = 
    let accessibility = Signature.Public

    let getSigFromFile file =
        let projectResults = parseAndCheckSingleFile file
        if projectResults.Errors.Length <> 0 then
            failwithf "found errors or warnings: %A" projectResults.Errors
        projectResults.AssemblySignature
        
    let sigA = a |> getSigFromFile
    let sigB = b |> getSigFromFile
    printfn "%A" (a.Trim())
    printfn "%A" cmd
    printfn "%A" (b.Trim())

    let declarationFilter (path: SignatureComparer.Path) (declaration : SignatureComparer.Declaration) = 
        let indent = String.replicate (path.Length) "  "
        printfn "%sPath: %A:" indent path
        printfn "%s%A" indent declaration
        true

    let result = SignatureComparer.compareAssemblySignature { DeclarationFilter = declarationFilter } (sigA, sigB)
    match cmd with
    | Equal -> result |> should be True
    | NotEqual -> result |> should be False

[<Fact>]
let testAll() =

    let testFile fn = 
        let testFile = File.ReadAllText fn
        let compareInstructions = 
            // normalize line endings and strip comments from lines beginning with "// --"
            let preprocessed = 
                testFile.Split([|"\n"|], StringSplitOptions.None)
                |> Array.map(fun s -> s.TrimEnd())
                |> Array.map(fun s -> if s.StartsWith("// --") then "// --" else s)
                |> String.concat "\n"

            preprocessed.Split([|"// --\n"|], StringSplitOptions.None)
            |> Seq.map makeCommand
            |> Seq.toList

        compareInstructions
        |> List.iter compare
    
    [
        ""
        "_Issues"
        "_Records"
        "_NamespacesAndModules"
    ]
    |> List.map (fun s -> "SignatureTestData" + s + ".fs_")
    |> List.iter testFile

