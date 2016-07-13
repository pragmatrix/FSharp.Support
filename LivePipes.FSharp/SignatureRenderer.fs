module LivePipes.FSharp.SignatureRenderer

#if false

open LivePipes.FSharp.SignatureFormatter

type StringFormattingStyle = {
    SpacesPerIndentLevel: int
}

let rec toStrings (style: StringFormattingStyle) =

    let indentPerLevel = String.replicate style.SpacesPerIndentLevel " "

    let render (c: Context) str = 
        let indent = String.replicate c.Level indentPerLevel
        indent + str

    let rec renderSignatures (signatures: FormattedSignature seq) : string seq = 

        let renderSignature signature = 
            match signature with
            | FormattedEntity(c, str, nested) ->
                seq {
                    yield render c str
                    yield! renderSignatures nested
                }
            | FormattedMember(c, str) ->
                render c str 
                |> Seq.singleton
    
        signatures
        |> Seq.map renderSignature
        |> Seq.collect id

    renderSignatures

#endif
