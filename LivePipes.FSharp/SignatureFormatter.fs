module LivePipes.FSharp.SignatureFormatter

#if false

open LivePipes.FSharp.Signature

type Context = {
    Path: EntitySignature list
    Level: int
}
with
    member this.Push(entity: EntitySignature) =
        {
            Path = entity :: this.Path
            Level = this.Level + 1
        }
    member this.IsRoot = List.isEmpty this.Path
    static member Root = { Path = []; Level = 0 }

type FormattedSignature =
    | FormattedEntity of Context * string * FormattedSignature seq
    | FormattedMember of Context * string

[<AutoOpen>]
module Helper =

    let ent c str nested = 
        FormattedEntity (c, str, nested)

    let mem c str = 
        FormattedMember (c, str)

let rec formatTypeSignature ts =
    let fts = formatTypeSignature
    match ts with
    | Tuple types ->
        [types |> List.map fts |> String.concat " * "]
    | Function (a, b) ->
        [fts a; " -> "; fts b]
    | Array (rank, t) ->
        [fts t; "["; String.replicate rank ","; "]"]
    | NamedType (QualifiedName n, types) ->
        match types with
        | [] -> [n]
        | _ -> [n; "<"; types |> List.map fts |> String.concat ","; ">"]
    | GenericParameter (GenericParameterReference(name, compileTime)) ->
        match compileTime with
        | true -> ["^"; name]
        | false -> ["'"; name]

    |> String.concat ""


let rec formatDeclarations (context: Context) (declarations: DeclarationSignature lset) : FormattedSignature seq = 

    let formatDeclaration = function
        | Entity entity -> formatEntity context entity
        | Member m -> formatMember context m

    declarations |> Seq.map formatDeclaration
    
and formatEntity (c: Context) (entity: EntitySignature) : FormattedSignature =
    
    match entity.Kind with
    | Module -> 
        ent 
            c 
            (sprintf "module %s%s" entity.Name (if c.IsRoot then "" else " ="))
            (formatDeclarations (c.Push entity) entity.Nested)
    | Abbrevation t ->
        ent
            c
            (sprintf "type %s = %s" entity.Name (formatTypeSignature t))
            []

and formatMember (c: Context) (m: MemberSignature) = 
    mem c ""

    
    








#endif
