/// This module generates comparable compilation unit signatures based on typed
/// FSharp graphs.
module LivePipes.FSharp.Signature

open System
open System.Collections
open System.Collections.Generic
open Microsoft.FSharp.Compiler.SourceCodeServices

[<Flags>]
type Visibility =
    | Public
    | Internal

type QualifiedName = QualifiedName of string

/// A list that is treated and compared as "unordered". This is
/// implemented by keeping the elements ordered.

[<StructuredFormatDisplay("LSet {ListStr}")>]
type 'a lset = 
    private LSet of 'a list
        member this.List = let (LSet(lst)) = this in lst
        member this.ListStr = sprintf "%A" this.List
        interface IEnumerable<'a> with
            member this.GetEnumerator() = 
                (this.List :> IEnumerable).GetEnumerator()
            member this.GetEnumerator() = 
                (this.List :> IEnumerable<'a>).GetEnumerator()

module LSet = 
    let ofList l =
        l 
        |> List.sort
        |> LSet

    let ofSeq l =
        l 
        |> Seq.toList
        |> ofList

    let toList (LSet l) = l 

/// A constant value represented by an obj that can be compared
[<CustomEquality; CustomComparison>]
type LiteralValue = 
    private LiteralValue of IComparable
    with
    interface IComparable with
        member this.CompareTo other =
            assert(other :? LiteralValue)
            let (LiteralValue me) = this
            match other with
            | :? LiteralValue as other ->
                let (LiteralValue other) = other
                me.CompareTo other
            | _ -> failwith "LiteralValue: CompareTo failed"
    override this.Equals other =
        (this :> IComparable).CompareTo(other) = 0
    override this.GetHashCode() = 
        let (LiteralValue me) = this
        me.GetHashCode()
    override this.ToString() =
        let (LiteralValue me) = this
        me.ToString()

    static member OfObj (v: obj) = 
        assert(not <| isNull v)
        match v with
        | :? IComparable as c -> LiteralValue c
        | _ -> failwithf "literal type %s is not comparable" (v.GetType().Name)

type Mutability = 
    | Immutable
    | Mutable

[<Flags>]
type MemberFlags = 
    | Abstract = 0x01
    | Static = 0x02 // or instance
    | Extension = 0x04
    | Override = 0x08
    | ExplicitInterfaceImplementation = 0x10

[<Flags>]
type FieldFlags = 
    | Mutable = 0x01
    | Literal = 0x02
    | Volatile = 0x04
    | DefaultValue = 0x08
    | Static = 0x10
    | CompilerGenerated = 0x20

type DeclarationSignature = 
    | Entity of EntitySignature
    | Member of MemberSignature

and EntitySignature = {
    Name: string
    Kind: EntityKindSignature
    Nested: DeclarationSignature lset
    Attributes: AttributeSignature lset
}

and EntityKindSignature = 
    | Module
    | Abbrevation of TypeSignature
    | Record of FieldSignature lset * GenericParameterSignature list
    | Union of UnionCaseSignature lset * GenericParameterSignature list
    | Measure
    | ExceptionDeclaration

    | Class of GenericParameterSignature list
    | Interface of GenericParameterSignature list
    | Array of rank: int * GenericParameterSignature list
    | Delegate of GenericParameterSignature list
    | Enum of FieldSignature lset
    | Value

and UnionCaseSignature = {
    Name: string
    Fields: FieldSignature list
    Attributes: AttributeSignature lset
}

and FieldSignature = {
    Name: string
    Flags: FieldFlags
    Type: TypeSignature
    // tbd: is the value part of the signature?
    Value: LiteralValue option
    PropertyAttributes: AttributeSignature lset
    FieldAttributes: AttributeSignature lset
}

and MemberSignature = {
    Name: string
    Type: TypeSignature
    Flags: MemberFlags
    Kind: MemberKindSignature
    Attributes: AttributeSignature lset
}

and MemberKindSignature =
    | Event
    | Property
    | MethodOrVal of literal: bool * Mutability * genericParameters: GenericParameterSignature list

and AttributeSignature = 
    AttributeSignature of TypeSignature * ConstructorArgument list * NamedArgument lset

and ConstructorArgument = 
    ConstructorArgument of TypeSignature * LiteralValue


and NamedArgument =
    NamedArgument of string * TypeSignature * LiteralValue
        member this.Name = let (NamedArgument (name, _, _)) = this in name

and GenericParameterSignature = 
    GenericParameterSignature of name: string * compileTime: bool * attributes: AttributeSignature lset * constraints: GenericParameterConstraintSignature lset

and GenericParameterReference = 
    GenericParameterReference of name: string * compileTime: bool

and GenericParameterConstraintSignature =
    | CoercesTo of TypeSignature
    | DefaultsTo of GenericParameterDefaultsToConstraintSignature
    | SupportsNull
    | MayResolveMember of GenericParameterMemberConstraintSignature
    | IsNonNullableStruct
    | IsReferenceType
    | SimpleChoice of TypeSignature lset
    | RequiresDefaultConstructor
    | IsEnum of TypeSignature
    | SupportsComparison
    | SupportsEquality
    | IsDelegate of GenericParameterDelegateConstraintSignature
    | IsUnmanaged

and GenericParameterDelegateConstraintSignature = {
    TupledArgumentType : TypeSignature
    ReturnType : TypeSignature
}

and GenericParameterMemberConstraintSignature = {
    Name: string
    IsStatic: bool
    // tbd: what are these and are they order dependent?
    // Sources: TypeSignature list
    ArgumentTypes: TypeSignature list
    ReturnType: TypeSignature
}

and GenericParameterDefaultsToConstraintSignature = {
    Priority: int
    Target: TypeSignature
}

and TypeSignature = 
    | Tuple of genericArguments: TypeSignature list
    | Function of TypeSignature * TypeSignature
    | Array of rank: int * TypeSignature
    | NamedType of QualifiedName * genericArguments: TypeSignature list
    | GenericParameter of GenericParameterReference

[<AutoOpen>]
module internal Helper =

    let inline map f = Seq.toList >> List.map f
    let inline ift v b = if b then v else enum(0)

    type FSharpAccessibility with
        member this.IsVisible a =
            match a, this with
            | Public, a when a.IsPublic -> true
            | Internal, a when a.IsPrivate || a.IsInternal -> true
            | _ -> false

    type FSharpType with
        member this.ResolveQualifiedName() : QualifiedName =
            if this.IsAbbreviation then
                this.AbbreviatedType.ResolveQualifiedName()
            elif not this.HasTypeDefinition then
                failwithf "failed to resolve qualified name for type %s" (this |> string)
            else
                this.TypeDefinition.ResolveQualifiedName()

    and FSharpEntity with

        member this.SignatureName = 
            this.LogicalName

        member this.ResolveQualifiedName() : QualifiedName = 
            if this.IsFSharpAbbreviation then
                this.AbbreviatedType.ResolveQualifiedName()
            else 
                this.FullName |> QualifiedName
    
let rec ofType (t: FSharpType) : TypeSignature =
    if t.IsTupleType then 
        Tuple (t.GenericArguments |> Seq.map ofType |> Seq.toList)
    elif t.IsFunctionType then 
        assert(t.GenericArguments.Count = 2)
        Function (t.GenericArguments.[0] |> ofType, t.GenericArguments.[1] |> ofType)
    elif t.HasTypeDefinition then
        ofEntityRef t.TypeDefinition (t.GenericArguments |> Seq.toList)
    elif t.IsGenericParameter then
        GenericParameter (ofGenericParameterReference t.GenericParameter)
    else failwith "unsupported F# type"
        
and ofEntityRef (t: FSharpEntity) (genericArguments: FSharpType list) : TypeSignature =
    if t.IsArrayType then
        if genericArguments.Length <> 1 then
            failwithf "expect exactly 1 generic argument for an array, seen %d" genericArguments.Length
        Array (t.ArrayRank, genericArguments.[0] |> ofType)
    else
        NamedType (t.ResolveQualifiedName(), genericArguments |> List.map ofType)

and ofConstructorArgument (t: FSharpType) (value: obj) = 
    ConstructorArgument (ofType t, LiteralValue.OfObj value)

and ofNamedArgument (name: string) (t: FSharpType) (value: obj) =
    NamedArgument (name, ofType t, LiteralValue.OfObj value)

and ofAttribute (attribute: FSharpAttribute) = 

    let constructorArguments =
        attribute.ConstructorArguments
        |> Seq.map (fun (t, v) -> ofConstructorArgument t v)
        |> Seq.toList

    let namedArguments = 
        attribute.NamedArguments
        |> Seq.map (fun (t, name, _ (* isField *), value) -> ofNamedArgument name t value)
        |> LSet.ofSeq

    AttributeSignature (ofEntityRef attribute.AttributeType [], constructorArguments, namedArguments)

and ofAttributes (attributes: FSharpAttribute seq) = 
    attributes
    |> map ofAttribute
    |> LSet.ofList

and ofGenericParameterDefaultsToConstraint (c: FSharpGenericParameterDefaultsToConstraint) =
    {
        Priority = c.DefaultsToPriority
        Target = c.DefaultsToTarget |> ofType
    }

and ofGenericParameterMemberConstraint (c: FSharpGenericParameterMemberConstraint) = 
    {
        Name = c.MemberName
        IsStatic = c.MemberIsStatic
        // Sources = c.MemberSources |> map ofType
        ArgumentTypes = c.MemberArgumentTypes |> map ofType
        ReturnType = c.MemberReturnType |> ofType
    }

and ofGenericParameterDelegateConstriant (c: FSharpGenericParameterDelegateConstraint) =
    {
        TupledArgumentType = c.DelegateTupledArgumentType |> ofType
        ReturnType = c.DelegateReturnType |> ofType
    }

and ofGenericParameterConstraint (c: FSharpGenericParameterConstraint) =
    match () with
    | _ when c.IsCoercesToConstraint ->
        CoercesTo <| ofType c.CoercesToTarget
    | _ when c.IsDefaultsToConstraint ->
        DefaultsTo <| ofGenericParameterDefaultsToConstraint c.DefaultsToConstraintData
    | _ when c.IsSupportsNullConstraint ->
        SupportsNull
    | _ when c.IsMemberConstraint ->
        MayResolveMember <| ofGenericParameterMemberConstraint c.MemberConstraintData
    | _ when c.IsNonNullableValueTypeConstraint ->
        IsNonNullableStruct
    | _ when c.IsReferenceTypeConstraint ->
        IsReferenceType
    | _ when c.IsSimpleChoiceConstraint ->
        SimpleChoice (c.SimpleChoices |> map ofType |> LSet.ofList)
    | _ when c.IsRequiresDefaultConstructorConstraint ->
        RequiresDefaultConstructor
    | _ when c.IsEnumConstraint ->
        IsEnum <| ofType c.EnumConstraintTarget
    | _ when c.IsComparisonConstraint ->
        SupportsComparison
    | _ when c.IsEqualityConstraint ->
        SupportsEquality
    | _ when c.IsDelegateConstraint ->
        IsDelegate <| ofGenericParameterDelegateConstriant c.DelegateConstraintData
    | _ when c.IsUnmanagedConstraint ->
        IsUnmanaged
    | _ -> failwith "unsupported F# parameter constraint"
            
and ofGenericParameter (p: FSharpGenericParameter) =
    GenericParameterSignature(
        p.Name,
        p.IsSolveAtCompileTime,
        p.Attributes |> ofAttributes,
        p.Constraints |> map ofGenericParameterConstraint |> LSet.ofList)

and ofGenericParameterReference (p: FSharpGenericParameter) = 
    GenericParameterReference(p.Name, p.IsSolveAtCompileTime)

let ofMemberOrFunctionOrValue (v: FSharpMemberOrFunctionOrValue) =
    let flags = 
        (v.IsDispatchSlot |> ift MemberFlags.Abstract)
        ||| (v.IsInstanceMember |> not |> ift MemberFlags.Static)
        ||| (v.IsExtensionMember |> ift MemberFlags.Extension)
        ||| ((not v.IsExplicitInterfaceImplementation && v.IsOverrideOrExplicitInterfaceImplementation) |> ift MemberFlags.ExplicitInterfaceImplementation)
        ||| (v.IsExplicitInterfaceImplementation |> ift MemberFlags.ExplicitInterfaceImplementation)

    let kind = 
        if v.IsEvent then
            Event 
        else if v.IsProperty then
            Property
        else 
            MethodOrVal (
                v.LiteralValue.IsSome, 
                (if v.IsMutable then Mutable else Immutable), 
                v.GenericParameters |> map ofGenericParameter)
    {
        Name = v.LogicalName
        Type = v.FullType |> ofType
        Flags = flags
        Kind = kind
        Attributes = v.Attributes |> map ofAttribute |> LSet.ofList
    }


let ofField (f: FSharpField) = 
    let flags = 
        (f.IsMutable |> ift FieldFlags.Mutable)
        ||| (f.IsLiteral |> ift FieldFlags.Literal)
        ||| (f.IsVolatile |> ift FieldFlags.Volatile)
        ||| (f.IsDefaultValue |> ift FieldFlags.DefaultValue)
        ||| (f.IsStatic |> ift FieldFlags.Static)
        ||| (f.IsCompilerGenerated |> ift FieldFlags.CompilerGenerated)
    
    {
        Name = f.Name
        Flags = flags
        Type = f.FieldType |> ofType
        Value = 
            if flags.HasFlag FieldFlags.Literal
            then Some <| LiteralValue.OfObj f.LiteralValue.Value
            else None
        PropertyAttributes = f.PropertyAttributes |> ofAttributes
        FieldAttributes = f.FieldAttributes |> ofAttributes
    }

let private ofFields (visibility: Visibility) (fields: FSharpField seq) =
    fields
    |> Seq.filter (fun f -> not f.IsCompilerGenerated && f.Accessibility.IsVisible visibility)
    |> map ofField
    |> LSet.ofList

let ofUnionCase (uc: FSharpUnionCase) = 
    {
        Name = uc.Name
        Fields = 
            uc.UnionCaseFields 
            |> map ofField
        Attributes = uc.Attributes |> ofAttributes
    }

let rec ofEntity 
    (visibility: Visibility) 
    (e: FSharpEntity) 
    (subDecls: FSharpImplementationFileDeclaration list) = 

    let kind =

        let genericParameters() = 
            e.GenericParameters |> map ofGenericParameter

        match () with
        | _ when e.IsFSharpModule -> 
            Module
        | _ when e.IsFSharpAbbreviation ->
            Abbrevation (ofType e.AbbreviatedType)
        | _ when e.IsFSharpRecord ->
            e.FSharpFields
            |> ofFields visibility
            |> fun fields -> Record (fields, genericParameters())
        | _ when e.IsFSharpUnion ->
            e.UnionCases 
            |> Seq.filter (fun uc -> uc.Accessibility.IsVisible visibility) 
            |> map ofUnionCase
            |> LSet.ofList
            |> fun cases -> Union (cases, genericParameters())
        | _ when e.IsMeasure -> Measure
        | _ when e.IsFSharpExceptionDeclaration -> ExceptionDeclaration
        | _ when e.IsClass -> Class (genericParameters())
        | _ when e.IsInterface -> Interface (genericParameters())
        | _ when e.IsArrayType ->
            EntityKindSignature.Array (e.ArrayRank, e.GenericParameters |> map ofGenericParameter)
        | _ when e.IsDelegate -> Delegate (genericParameters())
        | _ when e.IsEnum -> 
            e.FSharpFields
            |> ofFields visibility
            |> Enum
        | _ when e.IsValueType -> Value
        | _ -> failwith "unsupported F# type"

    let subSignatures =
        subDecls
        |> List.choose (fromDeclaration visibility)
        |> LSet.ofList

    {
        Name = e.SignatureName
        Kind = kind
        Nested = subSignatures
        Attributes = e.Attributes |> ofAttributes
    }

and private fromDeclaration visibility d =
    match d with 
    | FSharpImplementationFileDeclaration.Entity (e, subDecls) -> 

        if not (e.Accessibility.IsVisible visibility) then
            None
        else
        ofEntity visibility e subDecls
        |> Entity |> Some

    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, _, _) -> 
        if not (v.Accessibility.IsVisible visibility) || v.IsCompilerGenerated then
            None
        else
            ofMemberOrFunctionOrValue v |> Member |> Some

    | FSharpImplementationFileDeclaration.InitAction(e) -> 
        None

let create visibility (checkedFile: FSharpImplementationFileContents) : DeclarationSignature lset =
    checkedFile.Declarations
    |> List.choose (fromDeclaration visibility)
    |> LSet.ofList


    



