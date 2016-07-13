module SignatureFormatterTests

#if false

open System
open System.IO
open FsUnit
open Xunit
open LivePipes.FSharp

[<Fact>]
let testFormatting() = 
    let file = File.ReadAllText("SignatureTestFormats.fs")
    let parsed = parseAndCheckSingleFile file
    let fileContents = parsed.AssemblyContents.ImplementationFiles.[0]
    let declarationSignatures = Signature.create Signature.Visibility.Public fileContents
    let formatted = SignatureFormatter.formatDeclarations SignatureFormatter.Context.Root declarationSignatures
    let originalLines = file.Split('\n') |> Array.toList
    let printed = 
        SignatureRenderer.toStrings { SpacesPerIndentLevel = 4 } formatted
        |> Seq.toList

    printed |> should equal originalLines

#endif
