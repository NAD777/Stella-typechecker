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
  case typeError(description: TypeErrorDescription)
}

enum TypeErrorDescription {
  case custom(String)
  case listContainsDifferentTypes
  case typeMismatch(expectedType: StellaType, givenType: StellaType)
}

extension TypeErrorDescription: LocalizedError {
  var errorDescription: String? {
    switch self {
      case .custom(let string):
        return string
      case .typeMismatch(let expectedType, let givenType):
        return "Type mismatch: expected type \(expectedType.description), but \(givenType) was provided"
      case .listContainsDifferentTypes:
        return "The list must contain values of the same type"
    }
  }
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
        description.errorDescription
    }
  }
}

public func typecheck(program: Program) throws {
  var context = Context()

  try program.decls.forEach { context = try typecheck(decl: $0, context: context) }

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
          description: .custom("Return type of the function <\(name)> should be \(returnType!.description), got \(obtainedType.description)")
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
      return .bool

    case .constFalse:
      return .bool

    case .constUnit:
      return .unit

    case .constInt:
      return .nat

    case .constMemory(let mem):
      assertionFailure("Not implemented")

    case .var(let name):
      return try context.get(by: name)

    case .panic:
      return .bot

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
      let type = try typecheck(expr: n, context: context)
      guard type == .nat else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: type))
      }
      return .nat

    case .logicNot(let expr):
      let type = try typecheck(expr: expr, context: context)

      guard type == .bool else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .bool, givenType: type))
      }

      return .bool

    case .natPred(let n):
      assertionFailure("Not implemented")

    case .natIsZero(let n):
      let type = try typecheck(expr: n, context: context)
      guard type == .nat else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: type))
      }
      return .bool

    case .natRec(let n, let initial, let step):
      //    The typing of Nat::rec(n, initial, step) happens as follows:
      //
      //    n has to be of type Nat;
      //    initial can be of any type T;
      //    step has to be of type fn(Nat) -> (fn(T) -> T);

      let typeN = try typecheck(expr: n, context: context)
      guard typeN == .nat else {
        throw TypecheckError.typeError(
          description: .custom("First parameter in <Nat::rec> is expected to be Nat, got \(typeN.description)")
        )
      }

      let typeStep = try typecheck(expr: step, context: context)
      guard
        case let .fun(parameterTypesStep, returnTypeStep) = typeStep,
        parameterTypesStep[0] == .nat
      else {
        throw TypecheckError.typeError(
          description: .custom("Third parameter in <Nat::rec> is expected to be Fun(Nat) -> (Fun(T) -> T), got \(typeStep.description)")
        )
      }

      let typeInitial = try typecheck(expr: initial, context: context)
      guard
        case let .fun(lambdaTypes, lambdaReturnType) = returnTypeStep,
        lambdaTypes[0] == typeInitial,
        lambdaReturnType == typeInitial
      else {
        throw TypecheckError.typeError(
          description: .custom("Return type of the third parameter in <Nat::rec> is expected to be Fun(\(typeInitial.description)) -> \(typeInitial.description), got \(typeStep.description)")
        )
      }

      return typeInitial

    case .fix(let expr):
      assertionFailure("Not implemented")

    case .fold(let type, let expr):
      assertionFailure("Not implemented")

    case .unfold(let type, let expr):
      assertionFailure("Not implemented")

    case .application(let expr, let exprs):
      guard exprs.count == 1 else {
        throw TypecheckError.onlyOneArgument
      }

      let exprType = try typecheck(expr: expr, context: context)
      guard case let .fun(parameterTypes, returnType) = exprType else {
        throw TypecheckError.typeError(
          description: .custom("Expected TypeFun, got \(exprType.description)")
        )
      }

      let argType = try typecheck(expr: exprs[0], context: context)

      guard parameterTypes[0] == argType else {
        throw TypecheckError.typeError(
          description: .custom("Expected type \(parameterTypes[0].description) but got \(argType.description)")
        )
      }

      return returnType

    case .typeApplication(let expr, let types):
      assertionFailure("Not implemented")

    case .multiply(let left, let right),
        .divide(let left, let right),
        .subtract(let left, let right),
        .add(let left, let right):
      let typeLeft = try typecheck(expr: left, context: context)
      let typeRight = try typecheck(expr: right, context: context)

      guard typeLeft == .nat else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: typeLeft))
      }
      guard typeRight == .nat  else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: typeRight))
      }

      return .nat

    case .logicAnd(let left, let right),
        .logicOr(let left, let right):
      let typeLeft = try typecheck(expr: left, context: context)
      let typeRight = try typecheck(expr: right, context: context)

      guard typeLeft == .bool else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .bool, givenType: typeLeft))
      }
      guard typeRight == .bool  else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .bool, givenType: typeRight))
      }

      return .bool

    case .ref(let expr):
      let type = try typecheck(expr: expr, context: context)

      return .ref(type: type)

    case .deref(let expr):
      let type = try typecheck(expr: expr, context: context)

      guard case let .ref(refType) = type else {
        throw TypecheckError.typeError(
          description: .custom("Can dereference only Ref type, got \(type.description)")
        )
      }

    case .typeAsc(let expr, let type):
      assertionFailure("Not implemented")

    case .typeCast(let expr, let type):
      assertionFailure("Not implemented")

    case .abstraction(let paramDecls, let returnExpr):
      guard paramDecls.count == 1 else {
        throw TypecheckError.onlyOneArgument
      }

      let newContext = try context
        .add(paramDecls: paramDecls)

      let exprResultType = try typecheck(expr: returnExpr, context: newContext)

      return .fun(parameterTypes: paramDecls.map(\.type), returnType: exprResultType)

    case .tuple(let exprs):
      return try .tuple(types: exprs.map { try typecheck(expr: $0, context: context) })

    case .record(let bindings):
      assertionFailure("Not implemented")

    case .variant(let label, let rhs):
      assertionFailure("Not implemented")

    case .match(let expr, let cases):
      assertionFailure("Not implemented")

    case .list(let exprs):
      let listTypes = try exprs.map { try typecheck(expr: $0, context: context) }

      guard listTypes.allElementsEqual == true else {
        throw TypecheckError.typeError(description: .listContainsDifferentTypes)
      }

      return .list(types: listTypes)

    case .lessThan(let left, let right),
        .lessThanOrEqual(let left, let right),
        .greaterThanOrEqual(let left, let right),
        .greaterThan(let left, let right):
      let typeLeft = try typecheck(expr: left, context: context)
      let typeRight = try typecheck(expr: right, context: context)

      guard typeLeft == .nat else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: typeLeft))
      }
      guard typeRight == .nat  else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: typeRight))
      }

      return .bool

    case .notEqual(let left, let right),
        .equal(let left, let right):
      let leftType = try typecheck(expr: left, context: context)
      let rightType = try typecheck(expr: right, context: context)

      guard leftType == rightType else {
        throw TypecheckError.typeError(
          description: .custom("Lhs and rhs of equal exp must be the same type, got \(leftType.description) and  \(rightType.description)")
        )
      }

      return .bool

    case .assign(let lhs, let rhs):
      _ = try typecheck(expr: lhs, context: context)
      _ = try typecheck(expr: rhs, context: context)

      return .unit

    case .if(let condition, let thenExpr, let elseExpr):
      let conditionType = try typecheck(expr: condition, context: context)

      guard conditionType == .bool else {
        throw TypecheckError.typeError(
          description: .custom("If condition must have Bool type but \(conditionType.description) was provided")
        )
      }

      let thenExprType = try typecheck(expr: thenExpr, context: context)
      let elseExprType = try typecheck(expr: elseExpr, context: context)

      guard thenExprType == elseExprType else {
        throw TypecheckError.typeError(
          description: .custom("If's then and else blocks must have the same type")
        )
      }

      return thenExprType

    case .let(let patternBindings, let body):
      assertionFailure("Not implemented")

    case .letRec(let patternBindings, let body):
      assertionFailure("Not implemented")

    case .typeAbstraction(let generics, let expr):
      assertionFailure("Not implemented")

    case .sequence(let expr1, let expr2):
      assertionFailure("Not implemented")
  }
  fatalError("Not implemented")
}
