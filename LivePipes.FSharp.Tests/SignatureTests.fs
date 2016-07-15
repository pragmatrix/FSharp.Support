module SignatureTests

open System
open System.IO
open System.Diagnostics
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
let runMiniTests() =
    clearCaches()

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

[<Fact>]
let compareDependenciesOfFSharpCore() =
    clearCaches()
    let results1 = parseAndCheckSingleFile ""
    clearCaches()
    let results2 = parseAndCheckSingleFile ""
    
    let assemblies1 = results1.ProjectContext.GetReferencedAssemblies()
    let assemblies2 = results2.ProjectContext.GetReferencedAssemblies()

    assemblies1.Length |> should equal assemblies2.Length

    let compareAssemblySignatureWithItself (assemblies) =

        let declarationFilter (path: SignatureComparer.Path) (declaration : SignatureComparer.Declaration) = 
            let indent = String.replicate (path.Length) "  "
            let p = sprintf "%sPath: %A:" indent path
            let decl = sprintf "%s%A" indent declaration
            Debug.WriteLine(p)
            Debug.WriteLine(decl)
            true

        let r = SignatureComparer.compareAssemblySignature { DeclarationFilter = declarationFilter } assemblies
        r |> should be True
    
    Seq.zip assemblies1 assemblies2
    |> Seq.filter (fun (a, b) -> a.SimpleName = "FSharp.Core")
    |> Seq.map (fun (l, r) -> l.Contents, r.Contents)
    |> Seq.iter compareAssemblySignatureWithItself
