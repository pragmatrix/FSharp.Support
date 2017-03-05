module ProjectOptionsTests

open FsUnit
open Xunit
open FSharp.Support

[<Fact>]
let projectOptionsCanBeConvertedToFSharpOptionsAndBackWithoutLosingInformation() =
    let options = {
        ProjectFile = "test.fsproj"
        SourceFiles = ["Hello.fs"; "World.fs"]
        AssemblyReferences = ["a.dll"; "b.dll"]
        OutputFile = "c.dll"
        Options = ["--opt1"; "--opt2"]
    }

    let fs = options |> ProjectOptions.toFSharpOptions
    let o2 = fs |> ProjectOptions.fromFSharpOptions
    o2 |> should equal options

        

