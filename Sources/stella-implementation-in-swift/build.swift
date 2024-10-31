import Antlr4

public enum BuildError: Error {
    case UnexpectedParseContext(String)
}

public func buildParamDecl(ctx: stellaParser.ParamDeclContext) throws -> ParamDecl {
    return try ParamDecl(
        name: ctx.name.getText()!,
        type: buildType(ctx: ctx.paramType)
    )
}

public func buildType(ctx: stellaParser.StellatypeContext) throws -> StellaType {
    switch ctx {
    case is stellaParser.TypeBoolContext:
        return .bool
        
    case is stellaParser.TypeNatContext:
        return .nat
        
    case is stellaParser.TypeUnitContext:
        return .unit
        
    case let ctx as stellaParser.TypeFunContext:
        return try .fun(
            parameterTypes: ctx.paramTypes.map(buildType),
            returnType: buildType(ctx: ctx.returnType)
        )
    
    case let ctx as stellaParser.TypeForAllContext:
        return try .forAll(
            types: ctx.types.map { $0.getText()! },
            type: buildType(ctx: ctx.type_)
        )
    
    case let ctx as stellaParser.TypeSumContext:
        return try .sum(
            left: buildType(ctx: ctx.left),
            right: buildType(ctx: ctx.right)
        )
        
    case let ctx as stellaParser.TypeTupleContext:
        return try .tuple(
            types: ctx.types.map(buildType)
        )
        
    case let ctx as stellaParser.TypeListContext:
        return try .list(
              type: buildType(ctx: ctx.type_)
          )

    case let ctx as stellaParser.TypeRecordContext:
        return try .record(
            fieldTypes: ctx.fieldTypes.map{
                .init(label: $0.label.getText()!,
                      type: try buildType(ctx: $0.type_))
            }
        )
        
    case let ctx as stellaParser.TypeVariantContext:
        return try .variant(
            fieldTypes: ctx.fieldTypes.map{
                .init(label: $0.label.getText()!,
                      type: try $0.stellatype().map { try buildType(ctx: $0) } )
            }
        )
        
    case is stellaParser.TypeTopContext:
        return .top
     
    case let ctx as stellaParser.TypeRefContext:
        return try .ref(
            type: buildType(ctx: ctx.type_)
        )
        
    case is stellaParser.TypeBottomContext:
        return .bot
        
    case let ctx as stellaParser.TypeVarContext:
        return .var(
            name: ctx.name.getText()!
        )
        
    case let ctx as stellaParser.TypeParensContext:
        return try buildType(
            ctx: ctx.type_
        )
        
    default:
        throw BuildError.UnexpectedParseContext("not a type")
    }
}

public func buildExpr(ctx: stellaParser.ExprContext) throws -> Expr {
    switch ctx {
    case let ctx as stellaParser.DotRecordContext:
        return try .dotRecord(
            expr: buildExpr(ctx: ctx.expr_),
            label: ctx.label.getText()!
        )
    
    case let ctx as stellaParser.DotTupleContext:
        return try .dotTuple(
            expr: buildExpr(ctx: ctx.expr_),
            index: Int(ctx.index.getText()!)!
        )
        
    case is stellaParser.ConstTrueContext:
        return .constTrue
        
    case is stellaParser.ConstFalseContext:
        return .constFalse
        
    case is stellaParser.ConstUnitContext:
        return .constUnit
    
    case let ctx as stellaParser.ConstIntContext:
        return .constInt(
            value: Int(ctx.INTEGER()!.getText())!
        )
        
    case let ctx as stellaParser.ConstMemoryContext:
        #warning("TODO: Check, may be wrong")
        return .constMemory(
            mem: ctx.mem.getText()!
        )
        
    case let ctx as stellaParser.VarContext:
        return .var(
            name: ctx.name.getText()!
        )
        
    case is stellaParser.PanicContext:
        return .panic
        
    case let ctx as stellaParser.ThrowContext:
        return try .throw(
            expr: buildExpr(ctx: ctx.expr_)
        )
        
    case let ctx as stellaParser.TryCatchContext:
        return try .tryCatch(
            tryExpr: buildExpr(ctx: ctx.tryExpr),
            pat: buildPattern(ctx: ctx.pat),
            fallbackExpr: buildExpr(ctx: ctx.fallbackExpr)
        )
        
    case let ctx as stellaParser.TryWithContext:
        return try .tryWith(
            tryExpr: buildExpr(ctx: ctx.tryExpr),
            fallbackExpr: buildExpr(ctx: ctx.fallbackExpr)
        )
        
    case let ctx as stellaParser.InlContext:
        return try .inl(
            expr: buildExpr(ctx: ctx.expr_)
        )
        
    case let ctx as stellaParser.InrContext:
        return try .inr(
            expr: buildExpr(ctx: ctx.expr_)
        )
        
    case let ctx as stellaParser.ConsListContext:
        return try .listCons(
            head: buildExpr(ctx: ctx.head),
            tail: buildExpr(ctx: ctx.tail)
        )
        
    case let ctx as stellaParser.HeadContext:
        return try .listHead(
            list: buildExpr(ctx: ctx.list)
        )
    
    case let ctx as stellaParser.IsEmptyContext:
        return try .listIsEmpty(
            list: buildExpr(ctx: ctx.list)
        )
        
    case let ctx as stellaParser.TailContext:
        return try .listTail(
            list: buildExpr(ctx: ctx.list)
        )
        
    case let ctx as stellaParser.SuccContext:
        return try .succ(
            n: buildExpr(ctx: ctx.n)
        )
        
    case let ctx as stellaParser.LogicNotContext:
        return try .logicNot(
            expr: buildExpr(ctx: ctx.expr_)
        )
        
    case let ctx as stellaParser.PredContext:
        return try .natPred(
            n: buildExpr(ctx: ctx.n)
        )
    
    case let ctx as stellaParser.IsZeroContext:
        return try .natIsZero(
            n: buildExpr(ctx: ctx.n)
        )
        
    case let ctx as stellaParser.NatRecContext:
        return try .natRec(
            n: buildExpr(ctx: ctx.n),
            initial: buildExpr(ctx: ctx.initial),
            step: buildExpr(ctx: ctx.step)
        )
        
    case let ctx as stellaParser.FixContext:
        return try .fix(
            expr: buildExpr(ctx: ctx.expr_)
        )
    
    case let ctx as stellaParser.FoldContext:
        return try .fold(
            type: buildType(ctx: ctx.type_),
            expr: buildExpr(ctx: ctx.expr_)
        )
        
    case let ctx as stellaParser.UnfoldContext:
        return try .unfold(
            type: buildType(ctx: ctx.type_),
            expr: buildExpr(ctx: ctx.expr_)
        )
    
    case let ctx as stellaParser.ApplicationContext:
        return try .application(
            expr: buildExpr(ctx: ctx.fun),
            exprs: ctx.args.map(buildExpr)
        )
        
    case let ctx as stellaParser.TypeApplicationContext:
        return try .typeApplication(
            expr: buildExpr(ctx: ctx.fun),
            types: ctx.types.map(buildType)
        )
        
    case let ctx as stellaParser.MultiplyContext:
        return try .multiply(
            left: buildExpr(ctx: ctx.left),
            right: buildExpr(ctx: ctx.right)
        )
        
    case let ctx as stellaParser.DivideContext:
        return try .divide(
            left: buildExpr(ctx: ctx.left),
            right: buildExpr(ctx: ctx.right)
        )
        
    case let ctx as stellaParser.LogicAndContext:
        return try .logicAnd(
            left: buildExpr(ctx: ctx.left),
            right: buildExpr(ctx: ctx.right)
        )
        
    case let ctx as stellaParser.AddContext:
        return try .add(
            left: buildExpr(ctx: ctx.left),
            right: buildExpr(ctx: ctx.right)
        )
        
    case let ctx as stellaParser.SubtractContext:
        return try .subtract(
            left: buildExpr(ctx: ctx.left),
            right: buildExpr(ctx: ctx.right)
        )
        
    case let ctx as stellaParser.LogicOrContext:
        return try .logicOr(
            left: buildExpr(ctx: ctx.left),
            right: buildExpr(ctx: ctx.right)
        )
        
    case let ctx as stellaParser.RefContext:
        return try .ref(
            expr: buildExpr(ctx: ctx.expr_)
        )
        
    case let ctx as stellaParser.DerefContext:
        return try .deref(
            expr: buildExpr(ctx: ctx.expr_)
        )
        
    case let ctx as stellaParser.TypeAscContext:
        return try .typeAsc(
            expr: buildExpr(ctx: ctx.expr_),
            type: buildType(ctx: ctx.type_)
        )
        
    case let ctx as stellaParser.TypeCastContext:
        return try .typeCast(
            expr: buildExpr(ctx: ctx.expr_),
            type: buildType(ctx: ctx.type_)
        )
        
    case let ctx as stellaParser.AbstractionContext:
        return try .abstraction(
            paramDecls: ctx.paramDecls.map(buildParamDecl),
            returnExpr: buildExpr(ctx: ctx.returnExpr)
        )
        
    case let ctx as stellaParser.TupleContext:
        return try .tuple(
            exprs: ctx.exprs.map(buildExpr)
        )
        
    case let ctx as stellaParser.RecordContext:
        return .record(
            bindings: try ctx.bindings.map {
                .init(name: $0.name.getText()!, rhs: try buildExpr(ctx: $0.rhs))
            }
        )
        
    case let ctx as stellaParser.VariantContext:
        return try .variant(
            label: ctx.label.getText()!,
            rhs: ctx.expr().map(buildExpr)
        )
        
    case let ctx as stellaParser.MatchContext:
        return try .match(
            buildExpr(ctx: ctx.expr()!),
            cases: ctx.cases.map { .init(pattern: try buildPattern(ctx: $0.pattern_),
                                         expr: try buildExpr(ctx: $0.expr_)) }
        )
        
    case let ctx as stellaParser.ListContext:
        return try .list(
            exprs: ctx.exprs.map(buildExpr)
        )
        
    case let ctx as stellaParser.LessThanContext:
        return try .lessThan(
            left: buildExpr(ctx: ctx.left),
            right: buildExpr(ctx: ctx.right)
        )
    
    case let ctx as stellaParser.LessThanOrEqualContext:
        return try .lessThanOrEqual(
            left: buildExpr(ctx: ctx.left),
            right: buildExpr(ctx: ctx.right)
        )
        
    case let ctx as stellaParser.GreaterThanContext:
        return try .greaterThan(
            left: buildExpr(ctx: ctx.left),
            right: buildExpr(ctx: ctx.right)
        )
        
    case let ctx as stellaParser.GreaterThanOrEqualContext:
        return try .greaterThanOrEqual(
            left: buildExpr(ctx: ctx.left),
            right: buildExpr(ctx: ctx.right)
        )
        
    case let ctx as stellaParser.EqualContext:
        return try .equal(
            left: buildExpr(ctx: ctx.left),
            right: buildExpr(ctx: ctx.right)
        )
        
    case let ctx as stellaParser.NotEqualContext:
        return try .notEqual(
            left: buildExpr(ctx: ctx.left),
            right: buildExpr(ctx: ctx.right)
        )
        
    case let ctx as stellaParser.AssignContext:
        return try .assign(
            lhs: buildExpr(ctx: ctx.lhs),
            rhs: buildExpr(ctx: ctx.rhs)
        )
                                
    case let ctx as stellaParser.IfContext:
        return try .if(
            condition: buildExpr(ctx: ctx.condition),
            thenExpr: buildExpr(ctx: ctx.thenExpr),
            elseExpr: buildExpr(ctx: ctx.elseExpr)
        )
                            
    case let ctx as stellaParser.LetContext:
        return try .let(
            patternBindings: ctx.patternBindings.map { .init(pat: try buildPattern(ctx: $0.pat),
                                                             rhs: try buildExpr(ctx: $0.rhs)) },
            body: buildExpr(ctx: ctx.body)
        )
    
    case let ctx as stellaParser.LetRecContext:
        return try .letRec(
            patternBindings: ctx.patternBindings.map { .init(pat: try buildPattern(ctx: $0.pat),
                                                             rhs: try buildExpr(ctx: $0.rhs)) },
            body: buildExpr(ctx: ctx.body)
        )
        
    case let ctx as stellaParser.TypeAbstractionContext:
        return try .typeAbstraction(
            generics: ctx.generics.map { $0.getText()! },
            expr: buildExpr(ctx: ctx.expr_)
        )

    case let ctx as stellaParser.SequenceContext:
        return try .sequence(
            expr1: buildExpr(ctx: ctx.expr1),
            expr2: ctx.expr2 != nil ? buildExpr(ctx: ctx.expr2) : nil
        )
                            
    case let ctx as stellaParser.ParenthesisedExprContext:
        return try buildExpr(
            ctx: ctx.expr_
        )
    
    default:
        throw BuildError.UnexpectedParseContext("not an expr")
    }
}

public func buildPattern(ctx: stellaParser.PatternContext) throws -> Pattern {
    switch ctx {
    case let ctx as stellaParser.PatternVariantContext:
        return try .variant(label: ctx.label.getText()!, ctx.pattern().map { try buildPattern(ctx: $0)} )

    case let ctx as stellaParser.PatternInlContext:
        return try .inl(pat: buildPattern(ctx: ctx.pattern_))
        
    case let ctx as stellaParser.PatternInrContext:
        return try .inr(pat: buildPattern(ctx: ctx.pattern_))
        
    case let ctx as stellaParser.PatternTupleContext:
        return .tuple(patterns: try ctx.patterns.map(buildPattern))
        
    case let ctx as stellaParser.PatternRecordContext:
        return try .record(patterns: ctx.patterns.map{ .init(label: $0.label.getText()!, pattern: try buildPattern(ctx: $0.pattern_)) })
        
    case let ctx as stellaParser.PatternListContext:
        return .list(patterns: try ctx.patterns.map(buildPattern))
    
    case let ctx as stellaParser.PatternConsContext:
        return try .cons(head: buildPattern(ctx: ctx.head), tail: buildPattern(ctx: ctx.tail))
    
    case is stellaParser.PatternTrueContext:
        return .true
        
    case is stellaParser.PatternFalseContext:
        return .false
        
    case is stellaParser.PatternUnitContext:
        return .unit
        
    case let ctx as stellaParser.PatternIntContext:
        return .int(n: Int(ctx.n.getText()!)!)
        
    case let ctx as stellaParser.PatternSuccContext:
        return try .succ(n: buildPattern(ctx: ctx.pattern_))
        
    case let ctx as stellaParser.PatternVarContext:
        return .var(name: ctx.name.getText()!)
        
    case let ctx as stellaParser.ParenthesisedPatternContext:
        return try buildPattern(ctx: ctx.pattern_)
        
    default:
        throw BuildError.UnexpectedParseContext("not a pattern")
    }
}

public func buildDecl(ctx: stellaParser.DeclContext) throws -> Decl {
    switch ctx {
    case let ctx as stellaParser.DeclFunContext:
        return try .declFun(
            annotations: [], // TODO: annotations
            name: ctx.name.getText()!,
            paramDecls: ctx.paramDecls.map(buildParamDecl),
            returnType: ctx.returnType.map(buildType),
            throwTypes: ctx.throwTypes.map(buildType),
            localDecls: ctx.localDecls.map(buildDecl),
            returnExpr: buildExpr(ctx: ctx.returnExpr!)
        )
        
    case let ctx as stellaParser.DeclFunGenericContext:
        return try .declFunGeneric(
            annotations: [], // TODO: annotations
            name: ctx.name.getText()!,
            generics: ctx.generics.map { $0.getText()! },
            paramDecls: ctx.paramDecls.map(buildParamDecl),
            returnType: ctx.returnType.map(buildType),
            throwTypes: ctx.throwTypes.map(buildType),
            localDecls: ctx.localDecls.map(buildDecl),
            returnExpr: buildExpr(ctx: ctx.returnExpr!)
        )
        
    case let ctx as stellaParser.DeclTypeAliasContext:
        return try .declTypeAlias(
            name: ctx.name.getText()!,
            type: buildType(ctx: ctx.atype!)
        )
        
    case let ctx as stellaParser.DeclExceptionTypeContext:
        return try .declExceptionType(
            exceptionType: buildType(ctx: ctx.exceptionType)
        )
        
    case let ctx as stellaParser.DeclExceptionVariantContext:
        return try .declExceptionVariant(
            name: ctx.name.getText()!,
            variantType: buildType(ctx: ctx.variantType)
        )
        
    default:
        throw BuildError.UnexpectedParseContext("not a declaration")
    }
}

public func buildProgram(ctx: stellaParser.ProgramContext) throws -> Program {
    return try Program(
        languageDecl: .languageCore, // TODO: ctx.languageDecl()
        extensions: [], // TODO: ctx.extensions
        decls: ctx.decls.map(buildDecl)
    )
}
