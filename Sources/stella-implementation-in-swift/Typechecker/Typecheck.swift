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
  case notImplemented
  case typeError(description: TypeErrorDescription)
}

enum TypeErrorDescription {
  case custom(String)
  case listContainsDifferentTypes
  case expectedList
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
      case .expectedList:
        return "Expected list type"
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
      case .notImplemented:
        "Not implemented"
    }
  }
}

public func typecheck(program: Program) throws {
  let context = try Context()
    .add(decls: program.decls)

  try program.decls.forEach { try typecheck(decl: $0, context: context) }

  guard context.isMainPresent else {
    throw TypecheckError.missingMain
  }
}

func typecheck(decl: Decl, context: Context) throws {
  switch decl {
    case let .declFun(_, name, paramDecls, returnType, _, _, returnExpr):
      print("decl function, name = \(name)")
      let newContext = try context
        .add(paramDecls: paramDecls)

      let obtainedType = try typecheck(expr: returnExpr, expected: returnType, context: newContext)

      try assertEqual(expected: returnType, given: obtainedType)

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
}


func typecheck(expr: Expr, expected: StellaType?, context: Context) throws -> StellaType {
  switch expr {
    case .dotRecord(let expr, let label):
      let recordType = try typecheck(expr: expr, expected: nil, context: context)
      guard case let .record(fieldTypes) = recordType else {
          throw TypecheckError.typeError(
            description: .custom("DotRecord operator must be applied to record type, got \(recordType.description)")
          )
      }

      guard let fieldType = fieldTypes.first(where: { $0.label == label }) else {
          throw TypecheckError.typeError(
            description: .custom("No label \(label) in record \(recordType.description)")
          )
      }

      try assertEqual(expected: expected, given: fieldType.type)

      return fieldType.type

    case .dotTuple(let expr, let index):
      let tupleType = try typecheck(expr: expr, expected: nil, context: context)
      guard case let .tuple(types) = tupleType else {
          throw TypecheckError.typeError(
              description: .custom("DotTuple operator must be applied to pairs or tuples, got \(tupleType.description)")
          )
      }

      guard 0 < index, index <= types.count else {
          throw TypecheckError.typeError(
              description: .custom("Invalid index in dot Tuple")
          )
      }

      try assertEqual(expected: expected, given: types[index - 1])

      return types[index - 1]

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
      guard case .list(let exprs) = tail else {
        assertionFailure("Something wrong!")
        return .bot
      }
      let listExpr: Expr = .list(exprs: [head] + exprs)
      // TODO: do it more verbose
      return try typecheck(expr: listExpr, expected: expected, context: context)

    case .listHead(let list):
      let listType = try typecheck(expr: list, expected: nil, context: context)
      guard case .list(let type) = listType else {
        throw TypecheckError.typeError(description: .expectedList)
      }

      try assertEqual(expected: expected, given: type)

      return type

    case .listIsEmpty(let list):
      let type = try typecheck(expr: list, expected: nil, context: context)

      guard case .list(_) = type else {
        throw TypecheckError.typeError(description: .custom("Expected list type, but actual type is \(type.description)"))
      }

      try assertEqual(expected: expected, given: .bool)

      return .bool

    case .listTail(let list):
      let type = try typecheck(expr: list, expected: nil, context: context)

      guard case .list(_) = type else {
        throw TypecheckError.typeError(description: .custom("Expected list type, but actual type is \(type.description)"))
      }

      try assertEqual(expected: expected, given: type)

      return type

    case .succ(let n):
      let type = try typecheck(expr: n, expected: .nat, context: context)
      guard type == .nat else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: type))
      }
      return .nat

    case .logicNot(let expr):
      let type = try typecheck(expr: expr, expected: .bool, context: context)

      guard type == .bool else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .bool, givenType: type))
      }

      return .bool

    case .natPred(let n):
      assertionFailure("Not implemented")

    case .natIsZero(let n):
      let type = try typecheck(expr: n, expected: .nat, context: context)
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

      let typeN = try typecheck(expr: n, expected: .nat, context: context)
      guard typeN == .nat else {
        throw TypecheckError.typeError(
          description: .custom("First parameter in <Nat::rec> is expected to be Nat, got \(typeN.description)")
        )
      }

      let typeInitial = try typecheck(expr: initial, expected: nil, context: context)

      let typeStep = try typecheck(
        expr: step,
        expected: .fun(
          parameterTypes: [.nat],
          returnType: .fun(
            parameterTypes: [typeInitial],
            returnType: typeInitial
          )
        ), // fn(Nat) -> (fn(T) -> T)
        context: context
      )
      guard
        case let .fun(parameterTypesStep, returnTypeStep) = typeStep,
        parameterTypesStep[0] == .nat
      else {
        throw TypecheckError.typeError(
          description: .custom("Third parameter in <Nat::rec> is expected to be Fun(Nat) -> (Fun(T) -> T), got \(typeStep.description)")
        )
      }

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

      let funcType = try typecheck(expr: expr, expected: nil, context: context)
      guard case let .fun(parameterTypes, returnType) = funcType else {
        throw TypecheckError.typeError(
          description: .custom("Expected TypeFun, got \(funcType.description)")
        )
      }

      let argType = try typecheck(expr: exprs[0], expected: parameterTypes[0], context: context)

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
      let typeLeft = try typecheck(expr: left, expected: .nat, context: context)
      let typeRight = try typecheck(expr: right, expected: .nat, context: context)

      guard typeLeft == .nat else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: typeLeft))
      }
      guard typeRight == .nat  else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: typeRight))
      }

      return .nat

    case .logicAnd(let left, let right),
         .logicOr(let left, let right):
      let typeLeft = try typecheck(expr: left, expected: .bool, context: context)
      let typeRight = try typecheck(expr: right, expected: .bool, context: context)

      guard typeLeft == .bool else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .bool, givenType: typeLeft))
      }
      guard typeRight == .bool  else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .bool, givenType: typeRight))
      }

      return .bool

    case .ref(let expr):
      let type = try typecheck(expr: expr, expected: .nat, context: context) // TODO: nat?

      return .ref(type: type)

    case .deref(let expr):
      let type = try typecheck(expr: expr, expected: .ref(type: .nat), context: context) // TODO: ref-nat?

      guard case let .ref(refType) = type else {
        throw TypecheckError.typeError(
          description: .custom("Can dereference only Ref type, got \(type.description)")
        )
      }
      return refType

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

      let exprResultType = try typecheck(expr: returnExpr, expected: nil, context: newContext)

      return .fun(parameterTypes: paramDecls.map(\.type), returnType: exprResultType)

    case .tuple(let exprs):
      if let expected {
        guard case .tuple(let types) = expected else {
          throw TypecheckError.typeError(description: .custom("Expected not tuple type"))
        }
        guard types.count == exprs.count else {
          throw TypecheckError.typeError(description: .custom("Expected tuple of length \(types.count), but actual is \(exprs.count)"))
        }
        return try .tuple(
          types: zip(types, exprs).map {
            try typecheck(expr: $0.1, expected: $0.0, context: context)
          }
        )
      }
      return try .tuple(types: exprs.map { try typecheck(expr: $0, expected: nil, context: context) })

    case .record(let bindings):
      let recordNames = bindings.map(\.name)
      guard recordNames.allElementsUnique == true else {
        throw TypecheckError.typeError(
          description: .custom("Duplicate record type fields")
        )
      }

      return .record(
          fieldTypes: try bindings.map { RecordFieldType(
              label: $0.name,
              type: try typecheck(expr: $0.rhs, expected: nil, context: context) // TODO: expected?
          )}
      )

    case .variant(let label, let rhs):
      assertionFailure("Not implemented")

    case .match(let expr, let cases):
      assertionFailure("Not implemented")

    case .list(let exprs):
      let listTypes = try exprs.map { try typecheck(expr: $0, expected: nil, context: context) }

//      guard let firstListType = listTypes.first else {
//        throw TypecheckError.typeError(description: .listContainsDifferentTypes)
//      } TODO: think about ambiguous types
//      bad example, need to check
//      
//      fn ide(list : [Bool]) -> [Bool] {
//        return []
//      }
//      here we will have only [], without exprs, so, what is the type of the list?

      guard listTypes.allElementsEqual == true else {
        throw TypecheckError.typeError(description: .listContainsDifferentTypes)
      }
      
      if let expected {
        guard case .list(type: let expectedElementsType) = expected  else {
          throw TypecheckError.typeError(description: .custom("Expected type is not list"))
        }

        if listTypes.count == 0 {
          return .list(type: expectedElementsType)
        }

        guard let actualElementsType = listTypes.first else {
          assertionFailure("Something wrong! Impossible")
          return .top
        }

        guard actualElementsType == expectedElementsType else {
          throw TypecheckError.typeError(description: .custom("Expected elements of list is \(expectedElementsType), but actual type is \(actualElementsType)"))
        }
        // TODO: test this!
      }

      guard let first = listTypes.first else {
        throw TypecheckError.typeError(description: .custom("Error ambiguous list type"))
      }

      return .list(type: first)

    case .lessThan(let left, let right),
        .lessThanOrEqual(let left, let right),
        .greaterThanOrEqual(let left, let right),
        .greaterThan(let left, let right):
      let typeLeft = try typecheck(expr: left, expected: .nat, context: context)
      let typeRight = try typecheck(expr: right, expected: .nat, context: context)

      guard typeLeft == .nat else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: typeLeft))
      }
      guard typeRight == .nat  else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: typeRight))
      }

      return .bool

    case .notEqual(let left, let right),
         .equal(let left, let right):
      let leftType = try typecheck(expr: left, expected: nil, context: context)
      let rightType = try typecheck(expr: right, expected: nil, context: context)

      guard leftType == rightType else {
        throw TypecheckError.typeError(
          description: .custom("Lhs and rhs of equal exp must be the same type, got \(leftType.description) and  \(rightType.description)")
        )
      }

      return .bool

    case .assign(let lhs, let rhs):
      _ = try typecheck(expr: lhs, expected: nil, context: context)
      _ = try typecheck(expr: rhs, expected: nil, context: context)

      return .unit

    case .if(let condition, let thenExpr, let elseExpr):
      let conditionType = try typecheck(expr: condition, expected: .bool, context: context)

      guard conditionType == .bool else {
        throw TypecheckError.typeError(
          description: .custom("If condition must have Bool type but \(conditionType.description) was provided")
        )
      }

      let thenExprType = try typecheck(expr: thenExpr, expected: expected, context: context)
      let elseExprType = try typecheck(expr: elseExpr, expected: expected, context: context)

      guard thenExprType == elseExprType else {
        throw TypecheckError.typeError(
          description: .custom("If's then and else blocks must have the same type")
        )
      }

      return thenExprType

    case .let(let patternBindings, let body):
      guard let pattern = patternBindings.first,
            case .var(let name) = pattern.pat
      else {
        throw TypecheckError.notImplemented
      }

      let type = try typecheck(expr: pattern.rhs, expected: nil, context: context)
      let newContext = try context.add(name: name, type: type)

      return try typecheck(expr: body, expected: expected, context: newContext)

    case .letRec(let patternBindings, let body):
      assertionFailure("Not implemented")

    case .typeAbstraction(let generics, let expr):
      assertionFailure("Not implemented")

    case .sequence(let expr1, let expr2):
      assertionFailure("Not implemented")
  }
  fatalError("Not implemented")
}
