//
//  typecheck.swift
//  
//
//  Created by Nikolai Kudasov on 27.03.2023.
//

import Foundation

enum TypecheckError: Error {
  case missingMain
  case requiredReturnType
  case onlyOneArgument
  case typeError(description: String)
}

extension TypecheckError: LocalizedError {
  var errorDescription: String? {
    switch self {
      case .missingMain:
        "No main function in program"
      case .requiredReturnType:
        "Return type is required"
      case .onlyOneArgument:
        "Exactly one argument expected in function declaration"
      case .typeError(description: let description):
        description
    }
  }
}

public func typecheck(program: Program) throws {
  var context = Context()

  program.decls.forEach { context = try! typecheck(decl: $0, context: context) }

  guard context.isMainPresent else {
    throw TypecheckError.missingMain
  }
}

func typecheck(decl: Decl, context: Context) throws -> Context {
  switch decl {
    case let .declFun(_, name, paramDecls, returnType, _, _, returnExpr):
      print("decl function, name = \(name)")
      try context.assertNotPresent(for: name)
      let functionType = try typeForFunction(paramDecls: paramDecls, returnType: returnType)

      let returnedContext = try context
        .add(name: name, type: functionType)
        
      let newContext = try returnedContext
        .add(paramDecls: paramDecls)

      let obtainedType = try typecheck(expr: returnExpr, context: newContext)

      guard returnType == obtainedType else {
        throw TypecheckError.typeError(
          description: "Return type of the function <\(name)> should be \(returnType!.description), got \(obtainedType.description)"
        )
      }
      
      return returnedContext
    case let .declFunGeneric(_, name, _, _, _, _, _, _):
      print("decl function generic, name = \(name)")
      assertionFailure("Not implemented")
      
    case let .declTypeAlias(name, _):
      print("decl type alias, name = \(name)")
      assertionFailure("Not implemented")
      
    case .declExceptionType:
      print("decl exception type")
      assertionFailure("Not implemented")
      
    case let .declExceptionVariant(name, _):
      print("decl exception variant, name = \(name)")
      assertionFailure("Not implemented")
  }
  return context // TODO: make it right
}


func typecheck(expr: Expr, context: Context) throws -> StellaType {
  switch expr {
    case .dotRecord(let expr, let label):
      assertionFailure("Not implemented")

    case .dotTuple(let expr, let index):
      assertionFailure("Not implemented")

    case .constTrue:
      assertionFailure("Not implemented")

    case .constFalse:
      assertionFailure("Not implemented")

    case .constUnit:
      assertionFailure("Not implemented")

    case .constInt(let value):
      assertionFailure("Not implemented")

    case .constMemory(let mem):
      assertionFailure("Not implemented")

    case .var(let name):
      assertionFailure("Not implemented")

    case .panic:
      assertionFailure("Not implemented")

    case .throw(let expr):
      assertionFailure("Not implemented")

    case .tryCatch(let tryExpr, let pat, let fallbackExpr):
      assertionFailure("Not implemented")

    case .tryWith(let tryExpr, let fallbackExpr):
      assertionFailure("Not implemented")

    case .inl(let expr):
      assertionFailure("Not implemented")

    case .inr(let expr):
      assertionFailure("Not implemented")

    case .listCons(let head, let tail):
      assertionFailure("Not implemented")

    case .listHead(let list):
      assertionFailure("Not implemented")

    case .listIsEmpty(let list):
      assertionFailure("Not implemented")

    case .listTail(let list):
      assertionFailure("Not implemented")

    case .succ(let n):
      assertionFailure("Not implemented")

    case .logicNot(let expr):
      assertionFailure("Not implemented")

    case .natPred(let n):
      assertionFailure("Not implemented")

    case .natIsZero(let n):
      assertionFailure("Not implemented")

    case .natRec(let n, let initial, let step):
      assertionFailure("Not implemented")

    case .fix(let expr):
      assertionFailure("Not implemented")

    case .fold(let type, let expr):
      assertionFailure("Not implemented")

    case .unfold(let type, let expr):
      assertionFailure("Not implemented")

    case .application(let expr, let exprs):
      assertionFailure("Not implemented")

    case .typeApplication(let expr, let types):
      assertionFailure("Not implemented")

    case .multiply(let left, let right):
      assertionFailure("Not implemented")

    case .divide(let left, let right):
      assertionFailure("Not implemented")

    case .logicAnd(let left, let right):
      assertionFailure("Not implemented")

    case .add(let left, let right):
      assertionFailure("Not implemented")

    case .subtract(let left, let right):
      assertionFailure("Not implemented")

    case .logicOr(let left, let right):
      assertionFailure("Not implemented")

    case .ref(let expr):
      assertionFailure("Not implemented")

    case .deref(let expr):
      assertionFailure("Not implemented")

    case .typeAsc(let expr, let type):
      assertionFailure("Not implemented")

    case .typeCast(let expr, let type):
      assertionFailure("Not implemented")

    case .abstraction(let paramDecls, let returnExpr):
      assertionFailure("Not implemented")

    case .tuple(let exprs):
      assertionFailure("Not implemented")

    case .record(let bindings):
      assertionFailure("Not implemented")

    case .variant(let label, let rhs):
      assertionFailure("Not implemented")

    case .match(let expr, let cases):
      assertionFailure("Not implemented")

    case .list(let exprs):
      assertionFailure("Not implemented")

    case .lessThan(let left, let right):
      assertionFailure("Not implemented")

    case .lessThanOrEqual(let left, let right):
      assertionFailure("Not implemented")

    case .greaterThan(let left, let right):
      assertionFailure("Not implemented")

    case .greaterThanOrEqual(let left, let right):
      assertionFailure("Not implemented")

    case .equal(let left, let right):
      assertionFailure("Not implemented")

    case .notEqual(let left, let right):
      assertionFailure("Not implemented")

    case .assign(let lhs, let rhs):
      assertionFailure("Not implemented")

    case .if(let condition, let thenExpr, let elseExpr):
      assertionFailure("Not implemented")

    case .let(let patternBindings, let body):
      assertionFailure("Not implemented")

    case .letRec(let patternBindings, let body):
      assertionFailure("Not implemented")

    case .typeAbstraction(let generics, let expr):
      assertionFailure("Not implemented")

    case .sequence(let expr1, let expr2):
      assertionFailure("Not implemented")
  }
  return .bool
}

