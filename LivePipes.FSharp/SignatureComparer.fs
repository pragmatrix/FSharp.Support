module LivePipes.FSharp.SignatureComparer

open System.Diagnostics
open Microsoft.FSharp.Compiler.SourceCodeServices


type Path = (FSharpEntity * FSharpEntity) list

type Declaration =
    | Entity of FSharpEntity
    | Member of FSharpMemberOrFunctionOrValue
    | UnionCase of FSharpUnionCase
    | Field of FSharpField

type Visitor = {
    DeclarationFilter: Path -> Declaration -> bool
}

[<AutoOpen>]
module private Implementation =

    type 'a pair = 'a * 'a
    type 'a comparer = 'a pair -> bool

    type Context = {
        Visitor: Visitor
        mutable CurrentPath: Path
    }

    let scope (c: Context) (entityC : FSharpEntity comparer) : FSharpEntity comparer =
        fun args ->
            c.CurrentPath <- args :: c.CurrentPath
            let r = entityC args
            c.CurrentPath <- List.tail c.CurrentPath
            r
    
    let filter (declConstructor: 'a -> Declaration) (c: Context) (comparer: 'a seq comparer) : 'a seq comparer =
        let filter = c.Visitor.DeclarationFilter
        fun (lseq, rseq) ->
            let path = c.CurrentPath
            let filter = declConstructor >> filter path
            comparer (Seq.filter filter lseq, Seq.filter filter rseq)

    let nestedD get comparer = 
        fun (enclosingL, enclosingR) ->
            comparer() (get enclosingL, get enclosingR)

    let nested get comparer = 
        nestedD get (fun () -> comparer)

    let inline (<&>) pl pr v = if pl v then pr v else false

    /// Gets a boolean, compares them, and if both are true, gets the value
    /// and uses the delayed converter given for the value.
    let nestedIfD getB getValue delayedComparer =
        fun (enclosingL, enclosingR) ->
            let l, r = getB enclosingL, getB enclosingR
            match l, r with
            | false, false -> true
            | true, true -> (enclosingL, enclosingR) |> nested getValue (delayedComparer())
            | _ -> false

    /// Gets a boolean, compares them, and if both are true, gets the value
    /// and uses the converter given for the value.
    let nestedIf getB getValue comparer =
        nestedIfD getB getValue (fun () -> comparer)

    // build a member comparer that compares all members of a type in the order specified.
    let members<'a> (nested: 'a comparer list) : 'a comparer =
        fun pair ->
            nested
            |> List.forall (fun c -> c pair)

    // compare a sequence ignoring the order. The key specifies the relation and must be unique.
    let inline unorderedD<'a> key (comparer: unit -> 'a comparer) : 'a seq comparer =
        fun ((vl, vr): 'a seq pair) ->
            let l = vl |> Seq.toArray |> Array.sortBy key
            let r = vr |> Seq.toArray |> Array.sortBy key
            if l.Length <> r.Length then false
            else
            Array.zip l r 
            |> Array.forall (comparer())

    // compare a sequence ignoring the order. The key specifies the relation and must be unique.
    let inline unordered<'a> key (comparer: 'a comparer) : 'a seq comparer =
        unorderedD key (fun () -> comparer)

    // compare a sequence in order.
    let inline ordered<'a> (comparer: 'a comparer) : 'a seq comparer =
        fun ((vl, vr): 'a seq pair) ->
            let l = vl |> Seq.toArray
            let r = vr |> Seq.toArray
            if l.Length <> r.Length then false
            else
            Array.zip l r 
            |> Array.forall comparer

    let inline compare (a, b) = a = b

    let mutable compareType : FSharpType comparer = members []
    compareType <-

        let compareGenericParameterReference : FSharpGenericParameter comparer =
            members [
                nested (fun gp -> gp.Name) compare
                // nested (fun gp -> gp.IsCompilerGenerated) compare
                nested (fun gp -> gp.IsMeasure) compare
                nested (fun gp -> gp.IsSolveAtCompileTime) compare
                // nested (fun gp -> gp.Attributes)
                // nested (fun gp -> gp.Constraints) 
            ]

        let compareArrayType : FSharpType comparer =
            members [
                nested (fun t -> t.TypeDefinition.ArrayRank) compare
                nestedD (fun t -> t.GenericArguments) (fun () -> ordered compareType)
            ]
        
        let rec findAbbreviatedType (t: FSharpType) =
            if t.IsAbbreviation 
            then findAbbreviatedType t.AbbreviatedType
            else t
            
        fun ((l, r) as p) ->
            // first find the real types, if they are abbreviated
            let l, r = findAbbreviatedType l, findAbbreviatedType r
            match () with
            | _ when l.IsTupleType && r.IsTupleType -> 
                p 
                |> nestedD (fun t -> t.GenericArguments) (fun () -> ordered compareType)
            | _ when l.IsGenericParameter && r.IsGenericParameter ->
                p |> nested (fun t -> t.GenericParameter) compareGenericParameterReference                
            // now we must have a type definition
            | _ when l.HasTypeDefinition && r.HasTypeDefinition ->
                let ltd, rtd = l.TypeDefinition, r.TypeDefinition
                if ltd.IsArrayType && rtd.IsArrayType then
                    p |> compareArrayType
                else
                let lfn, rfn = ltd.TryFullName, rtd.TryFullName
                if lfn.IsNone || rfn.IsNone then false
                else lfn.Value = rfn.Value
            | _ -> false
(*
        members [
            nestedIf (fun t -> t.HasTypeDefinition) (fun t -> t.TypeDefinition) compareTypeDefinition
            nestedIfD (fun t -> t.IsTupleType) (fun t -> t.GenericArguments) (fun () -> ordered compareType)
            nestedIfD (fun t -> t.HasTypeDefinition && t.TypeDefinition.IsArrayType) id (fun () -> compareArrayType)
            nestedIfD (fun t -> t.IsAbbreviation) (fun t -> t.AbbreviatedType) (fun () -> compareType)
            nestedIf (fun t -> t.IsGenericParameter) (fun t -> t.GenericParameter) compareGenericParameterReference
        ]
*)

    type ConstructorArgument = FSharpType * obj
    type NamedArgument = FSharpType * string * bool (*isField*) * obj (*value*)

    let compareAttributes : FSharpAttribute seq comparer = 

        let compareAttribute : FSharpAttribute comparer = 

            let compareConstructorArgument = 
                members<ConstructorArgument> [
                    nested (fun (t, _) -> t) compareType
                    nested (fun (_, v) -> v) compare
                ]

            let compareConstructorArguments = 
                ordered compareConstructorArgument

            let compareNamedArgument = 
                members<NamedArgument> [
                    nested (fun (t, _ , _, _) -> t) compareType
                    nested (fun (_, n , _, _) -> n) compare
                    nested (fun (_, _ , _, v) -> v) compare
                ]
            let compareNamedArguments = 
                unordered (fun (_, name, _, _)-> name) compareNamedArgument

            members [
                nested (fun a -> a.AttributeType.FullName) compare
                nested (fun a -> a.ConstructorArguments) compareConstructorArguments
                nested (fun a -> a.NamedArguments) compareNamedArguments
            ]
    
        ordered compareAttribute

    let compareGenericParameterDeclaration = 

        let compareGenericParameterConstraint : FSharpGenericParameterConstraint comparer =

            let compareDefaultsToConstraint : FSharpGenericParameterDefaultsToConstraint comparer =
                members [
                    nested (fun dtc -> dtc.DefaultsToPriority) compare
                    nested (fun dtc -> dtc.DefaultsToTarget) compareType
                ]

            let compareMemberConstraint : FSharpGenericParameterMemberConstraint comparer =
                members [
                    nested (fun mc -> mc.MemberName) compare
                    nested (fun mc -> mc.MemberIsStatic) compare
                    nested (fun mc -> mc.MemberSources) (ordered compareType)
                    nested (fun mc -> mc.MemberArgumentTypes) (ordered compareType)
                    nested (fun mc -> mc.MemberReturnType) compareType
                ]

            let compareDelegateConstraint : FSharpGenericParameterDelegateConstraint comparer =
                members [
                    nested (fun dc -> dc.DelegateTupledArgumentType) compareType
                    nested (fun dc -> dc.DelegateReturnType) compareType
                ]

            members [
                nestedIf (fun c -> c.IsCoercesToConstraint) (fun c -> c.CoercesToTarget) compareType
                nestedIf (fun c -> c.IsDefaultsToConstraint) (fun c -> c.DefaultsToConstraintData) compareDefaultsToConstraint
                nested (fun c -> c.IsSupportsNullConstraint) compare
                nestedIf (fun c -> c.IsMemberConstraint) (fun c -> c.MemberConstraintData) compareMemberConstraint
                nested (fun c -> c.IsNonNullableValueTypeConstraint) compare
                nested (fun c -> c.IsReferenceTypeConstraint) compare
                nestedIf (fun c -> c.IsSimpleChoiceConstraint) (fun c -> c.SimpleChoices) (unordered (fun t -> t.TypeDefinition.FullName) compareType)
                nested (fun c -> c.IsRequiresDefaultConstructorConstraint) compare
                nested (fun c -> c.IsEnumConstraint) compare
                nested (fun c -> c.IsComparisonConstraint) compare
                nested (fun c -> c.IsEqualityConstraint) compare
                nested (fun c -> c.IsUnmanagedConstraint) compare
                nestedIf (fun c -> c.IsDelegateConstraint) (fun c -> c.DelegateConstraintData) compareDelegateConstraint
            ]

        let compareGenericParameterConstraints : FSharpGenericParameterConstraint seq comparer =
            // probably unordered, but we can't sort them without a proper key
            ordered compareGenericParameterConstraint

        members<FSharpGenericParameter> [
            // names are not relevant, only depend on the order inside the entity declaration
            // (also these names might be compiler generated if IsCompilerGenerated is true)
            // nested (fun gp -> gp.Name) compare
            nested (fun gp -> gp.IsMeasure) compare
            nested (fun gp -> gp.IsSolveAtCompileTime) compare
            nested (fun gp -> gp.Attributes) compareAttributes
            nested (fun gp -> gp.Constraints) compareGenericParameterConstraints
        ]

    let compareGenericParameterDeclarations = 
        ordered compareGenericParameterDeclaration

    let compareAccessibility =
        members<FSharpAccessibility> [
            nested (fun a -> a.IsInternal, a.IsPrivate, a.IsPublic) compare
        ]

    let compareMember : FSharpMemberOrFunctionOrValue comparer = 
        members<FSharpMemberOrFunctionOrValue> [
            nested (fun e -> e.GenericParameters) compareGenericParameterDeclarations
            nested (fun e -> e.FullType) compareType
            nested (fun e -> e.InlineAnnotation) compare
            nested (fun e -> e.IsMutable) compare
            nested (fun e -> e.IsModuleValueOrMember) compare
            nested (fun e -> e.IsMember) compare
            nested (fun e -> e.IsDispatchSlot) compare
            nestedIf (fun e -> e.IsProperty) (fun e -> e.HasGetterMethod, e.HasSetterMethod) compare
            nested (fun e -> e.IsEvent) compare
            nested (fun e -> e.IsInstanceMember) compare
            nested (fun e -> e.IsExtensionMember) compare
            nested (fun e -> e.IsExplicitInterfaceImplementation) compare
            nested (fun e -> e.IsActivePattern) compare
            nested (fun e -> e.CompiledName) compare
            nested (fun e -> e.Attributes) compareAttributes
            // implementation only:
            // nested (fun e -> e.IsBaseValue) compare
            // nested (fun e -> e.IsConstructorThisValue) compare
            // nested (fun e -> e.IsMemberThisValue) compare
            nested (fun e -> e.LiteralValue) compare
            nested (fun e -> e.Accessibility) compareAccessibility
        ]
    
    let compareEntity (context: Context) =

        let compareDelegateArgument =
            members<string option * FSharpType>[
                nested fst compare
                nested snd compareType
            ]

        let compareDelegateSignature = 
            members<FSharpDelegateSignature> [
                nested (fun ds -> ds.DelegateArguments) (ordered compareDelegateArgument)
                nested (fun ds -> ds.DelegateReturnType) compareType
            ]

        let compareBaseType =
            members<FSharpEntity> [
                nestedIf (fun e -> e.BaseType.IsSome) (fun e -> e.BaseType.Value) compareType
            ]

        let compareMembers =
            let compareMembers : FSharpMemberOrFunctionOrValue seq comparer =
                unordered<FSharpMemberOrFunctionOrValue> (fun m -> m.CompiledName) compareMember
                |> filter Member context

            members<FSharpEntity> [
                nested (fun e -> e.MembersFunctionsAndValues) compareMembers
            ]

        let compareDeclaredInterfaces =
            members<FSharpEntity> [
                nested 
                    (fun e -> e.DeclaredInterfaces) 
                    (unordered (fun t -> t.TypeDefinition.CompiledName) compareType)
            ]
    
        let mutable compareEntity = members []

        let compareNestedEntities =
            unorderedD<FSharpEntity> (fun e -> e.CompiledName) (fun () -> compareEntity)
            |> filter Entity context

        let compareModule : FSharpEntity comparer = 
            members [
                nested (fun e -> e.HasFSharpModuleSuffix) compare
                nested id compareMembers
                nested (fun e -> e.NestedEntities) compareNestedEntities
            ]

        let compareField : FSharpField comparer =
            members [
                nested (fun f -> f.Name) compare

                nested (fun f -> f.IsMutable) compare
                nestedIf (fun f -> f.IsLiteral) (fun f -> f.LiteralValue) compare
                nested (fun f -> f.IsVolatile) compare
                nested (fun f -> f.IsDefaultValue) compare
                nested (fun f -> f.FieldType) compareType
                nested (fun f -> f.IsStatic) compare
                nested (fun f -> f.FieldAttributes) compare
                nested (fun f -> f.PropertyAttributes) compare
                nested (fun f -> f.Accessibility) compare
            ]

        let compareRecordFields : FSharpEntity comparer =
            members [
                nested (fun e -> e.FSharpFields) (unordered<FSharpField> (fun f -> f.Name) compareField |> filter Field context)
            ]
        
        let compareUnionCases : FSharpEntity comparer =

            let compareCase : FSharpUnionCase comparer =
                members [
                    nested (fun uc -> uc.Name) compare
                    nested (fun uc -> uc.UnionCaseFields) (ordered compareField |> filter Field context)
                    nested (fun uc -> uc.Attributes) compareAttributes
                    nested (fun uc -> uc.Accessibility) compareAccessibility
                ]

            let compareCases : FSharpUnionCase seq comparer =
                unordered<FSharpUnionCase> (fun uc -> uc.Name) compareCase
                |> filter UnionCase context

            members [
                nested (fun e -> e.UnionCases) compareCases
            ]

        compareEntity <- members<FSharpEntity> [
            nested (fun e -> e.Accessibility) compareAccessibility

            nested (fun e -> e.CompiledName) compare
            // don't need to compare the full name, because we have compared parents before,
            // also type abbrevations do not have a FullName
            // nested (fun e -> e.FullName) compare
            nested (fun e -> e.GenericParameters) (compareGenericParameterDeclarations)
            nestedIf (fun e -> e.IsFSharpModule) id compareModule
            nestedIf (fun e -> e.IsArrayType) (fun e -> e.ArrayRank) compare
            nested (fun e -> e.IsByRef) compare

            nested (fun e -> e.IsEnum) compare
            // ValueType is always set when IsEnum is true
            nestedIf (fun e -> e.IsValueType) id (compareMembers <&> compareDeclaredInterfaces)

            nestedIf (fun e -> e.IsDelegate) (fun e -> e.FSharpDelegateSignature) compareDelegateSignature
            nestedIf (fun e -> e.IsInterface) id (compareBaseType <&> compareDeclaredInterfaces <&> compareMembers)
            nestedIf (fun e -> e.IsClass) id (compareBaseType <&> compareDeclaredInterfaces <&> compareMembers)

            nested (fun e -> e.IsMeasure) compare
            nested (fun e -> e.IsFSharpExceptionDeclaration) compare
            nestedIf (fun e -> e.IsFSharpAbbreviation) (fun e -> e.AbbreviatedType) compareType

            nestedIf (fun e -> e.IsFSharpRecord) id (compareRecordFields <&> compareDeclaredInterfaces <&> compareMembers)
            nestedIf (fun e -> e.IsFSharpUnion) id (compareUnionCases <&> compareDeclaredInterfaces <&> compareMembers)

            nested (fun e -> e.Attributes) compareAttributes
        ] |> scope context

        compareEntity

    let compareEntities context = 
        let compareEntity = compareEntity context
        unordered<FSharpEntity> (fun e -> e.CompiledName) compareEntity
        |> filter Entity context

let compareAssemblySignature (v : Visitor) : FSharpAssemblySignature comparer = 
    let c = { Visitor = v; CurrentPath = [] }
    let compareEntities = compareEntities c
    members [
        nested (fun s -> s.Attributes) compareAttributes
        nested (fun s -> s.Entities) compareEntities
    ] 
