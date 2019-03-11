// Copyright 2018 Fabulous contributors. See LICENSE.md for license.

module FSharp.Compiler.PortaCode.CodeModel

type DRange = 
   { File: string 
     StartLine: int
     StartColumn: int
     EndLine: int
     EndColumn: int }

/// A representation of resolved F# expressions that can be serialized
type DExpr = 
    | Value  of DLocalRef
    | ThisValue  of DType 
    | BaseValue  of DType 
    | Application of DExpr * DType[] * DExpr[] * DRange option 
    | Lambda of DType * DType * DLocalDef * DExpr  
    | TypeLambda of DGenericParameterDef[] * DExpr  
    | Quote  of DExpr  
    | IfThenElse   of DExpr * DExpr * DExpr  
    | DecisionTree   of DExpr * (DLocalDef[] * DExpr)[]
    | DecisionTreeSuccess of int * DExpr[]
    | Call of DExpr option * DMemberRef * DType[] * DType[] * DExpr[] * DRange option
    | NewObject of DMemberRef * DType[] * DExpr[] 
    | LetRec of ( DLocalDef * DExpr)[] * DExpr  
    | Let of (DLocalDef * DExpr) * DExpr 
    | NewRecord of DType * DExpr[] 
    | ObjectExpr of DType * DExpr * DObjectExprOverrideDef[] * (DType * DObjectExprOverrideDef[])[]
    | FSharpFieldGet of  DExpr option * DType * DFieldRef 
    | FSharpFieldSet of  DExpr option * DType * DFieldRef * DExpr 
    | NewUnionCase of DType * DUnionCaseRef * DExpr[]  
    | UnionCaseGet of DExpr * DType * DUnionCaseRef * DFieldRef 
    | UnionCaseSet of DExpr * DType * DUnionCaseRef * DFieldRef  * DExpr
    | UnionCaseTag of DExpr * DType 
    | UnionCaseTest of DExpr  * DType * DUnionCaseRef 
    | TraitCall of DType[] * string * isInstance: bool * DType[] * DType[] * DExpr[] * DRange option
    | NewTuple of DType * DExpr[]  
    | TupleGet of DType * int * DExpr 
    | Coerce of DType * DExpr  
    | NewArray of DType * DExpr[]  
    | TypeTest of DType * DExpr  
    | AddressSet of DExpr * DExpr  
    | ValueSet of Choice<DLocalRef, DMemberRef> * DExpr  
    | Unused
    | DefaultValue of DType  
    | Const of obj * DType
    | AddressOf of DExpr 
    | Sequential of DExpr * DExpr  
    | FastIntegerForLoop of DExpr * DExpr * DExpr * bool
    | WhileLoop of DExpr * DExpr  
    | TryFinally of DExpr * DExpr  
    | TryWith of DExpr * DLocalDef * DExpr * DLocalDef * DExpr  
    | NewDelegate of DType * DExpr  
    | ILFieldGet of DExpr option * DType * string 
    | ILFieldSet of DExpr option * DType * string  * DExpr 
    | ILAsm of string * DType[] * DExpr[]

and DType = 
    | DNamedType of DEntityRef * DType[]
    | DFunctionType of DType * DType
    | DTupleType of bool * DType[]
    | DArrayType of int * DType
    | DByRefType of DType
    | DVariableType of string

and DLocalDef = 
    { Name: string
      IsMutable: bool
      Type: DType
      Range: DRange option
      IsCompilerGenerated: bool }

and DMemberDef = 
    { EnclosingEntity: DEntityRef
      Name: string
      GenericParameters: DGenericParameterDef[]
      IsInstance: bool
      IsValue: bool
      IsCompilerGenerated: bool
      Parameters: DLocalDef[]
      ReturnType: DType
      Range: DRange option }

    member x.Ref = 
        { Entity=x.EnclosingEntity
          Name= x.Name
          GenericArity = x.GenericParameters.Length 
          ArgTypes = (x.Parameters |> Array.map (fun p -> p.Type)) 
          ReturnType = x.ReturnType 
          Range = x.Range }

and DGenericParameterDef = 
    { Name: string }

and DEntityDef = 
    { Name: string
      GenericParameters: DGenericParameterDef[]
      UnionCases: string[]
      Range: DRange option }

and DEntityRef = DEntityRef of string 

and DMemberRef = 
    { Entity: DEntityRef 
      Name: string
      GenericArity: int 
      ArgTypes: DType[] 
      ReturnType: DType 
      Range: DRange option }

and DLocalRef = 
    { Name: string
      IsThisValue: bool
      IsMutable: bool
      Range: DRange option }

and DFieldRef = DFieldRef of int * string

and DUnionCaseRef = DUnionCaseRef of string

and DObjectExprOverrideDef =  
    { Name: string
      GenericParameters: DGenericParameterDef[]
      Parameters: DLocalDef[]
      Body: DExpr }

type DDecl = 
    | DDeclEntity of DEntityDef * DDecl[]
    | DDeclMember of DMemberDef * DExpr * isLiveCheck: bool
    | InitAction of DExpr * DRange option

type DFile = 
    { Code: DDecl[] }

