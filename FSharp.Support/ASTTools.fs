/// Tools for the untyped AST
module FSharp.Support.ASTTools

open Microsoft.FSharp.Compiler.Ast

module RemoveEntryPointAttribute =

    let rewriteBinding (Binding(access, bindingKind, mustInline, isMutable, attributes, xmlDoc, valData, headPat, returnInfo, expr, lhsRange, spBind)) =
        Binding(access, bindingKind, mustInline, isMutable, attributes, xmlDoc, valData, headPat, returnInfo, expr, lhsRange, spBind)

    let rec rewriteDeclaration (declaration: SynModuleDecl) : SynModuleDecl =
        match declaration with
        | SynModuleDecl.NestedModule (sci, isRec, declarations, b, r) -> 
            SynModuleDecl.NestedModule (sci, isRec, declarations |> List.map rewriteDeclaration, b, r)
        | SynModuleDecl.Let (b, bindings, r) ->
            SynModuleDecl.Let (b, bindings |> List.map rewriteBinding, r)
        | SynModuleDecl.NamespaceFragment moduleOrNamespace ->
            SynModuleDecl.NamespaceFragment (rewriteModuleOrNamespace moduleOrNamespace)
        | _ -> declaration

    and rewriteModuleOrNamespace (SynModuleOrNamespace(lid, isRec, isModule, declarations, xmlDoc, attributes, synAccess, range)) =
        let declarations = declarations |> List.map rewriteDeclaration
        (SynModuleOrNamespace(lid, isRec, isModule, declarations, xmlDoc, attributes, synAccess, range))
    
    let rewriteParsedInput (parsedInput: ParsedInput) = 
        match parsedInput with
        | ParsedInput.ImplFile(implFile) ->
            let (ParsedImplFileInput(filename, isScript, name, scopedPragmas, parsedHashDirectives, modules, twoFlags)) = implFile
            let modules = modules |> List.map rewriteModuleOrNamespace
            ParsedImplFileInput(filename, isScript, name, scopedPragmas, parsedHashDirectives, modules, twoFlags)
            |> ParsedInput.ImplFile

        | _ -> parsedInput
    