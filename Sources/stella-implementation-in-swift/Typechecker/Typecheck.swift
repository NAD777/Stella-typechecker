//
//  typecheck.swift
//  
//
//  Created by Nikolai Kudasov on 27.03.2023.
//

import Foundation

enum Config {
  enum Mode {
    case debug
    case prod
  }

  static let mode: Mode = .prod
}

func _print(_ items: Any..., separator: String = " ", terminator: String = "\n") {
  guard Config.mode == .debug else { return }

  print(items, separator: separator, terminator: terminator)
}

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
  case expectedList(actualType: StellaType)
  case unexpectedList(expectedType: StellaType)
  case ambiguousListType
  case typeMismatch(expectedType: StellaType, givenType: StellaType)
  case illegalEmptyMatching
  case unexpectedPatternForType
  case nonExhaustiveMatchPatterns
  case notAFunction(actualType: StellaType)
  case notATuple(actualType: StellaType)
  case unexpectedVariantLabel
  case ambiguousVariantType
  case exceptionTypeNotDefined(exprType: StellaType)
  case exceptionTypeNotDefinedGlobally
  case ambiguousThrowType
  case ambiguousPanicType
  case unexpectedLambda
  case tupleIndexOutOfBounds(index: Int)
  case unexpectedTuple
  case unexpectedVariant
  case ambiguousReferenceType
  case unexpectedMemoryAddress
  case notAReference(actualType: StellaType)
  case unexpectedFieldAccess(fieldName: String, recordType: StellaType)
  case unexpectedInjection(expectedType: StellaType)
  case ambiguousSumType
  case unexpectedTupleLength(expected: Int, actual: Int)
  case unexpectedRecordType(expectedType: StellaType)
}

extension TypeErrorDescription: LocalizedError {
  var errorDescription: String? {
    switch self {
      case .custom(let string):
        return string
      case .typeMismatch(let expectedType, let givenType):
        return "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: Type mismatch: expected type `\(expectedType.description)`, but `\(givenType.description)` was provided"
      case .listContainsDifferentTypes:
        return "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: The list must contain values of the same type"
      case .expectedList(let actualType):
        return "ERROR_NOT_A_LIST: Expected list type, but actual type is `\(actualType.description)`"
      case .illegalEmptyMatching:
        return "ERROR_ILLEGAL_EMPTY_MATCHING: Illegal empty matching"
      case .unexpectedPatternForType:
        return "ERROR_UNEXPECTED_PATTERN_FOR_TYPE: Unexpected pattern for type"
      case .nonExhaustiveMatchPatterns:
        return "ERROR_NONEXHAUSTIVE_MATCH_PATTERNS: Nonexhaustive match patterns"
      case .notAFunction(let actualType):
        return "ERROR_NOT_A_FUNCTION: Expected a function type but got \(actualType.description)"
      case .unexpectedVariantLabel:
        return "ERROR_UNEXPECTED_VARIANT_LABEL: Unexpected variant label"
      case .ambiguousVariantType:
        return "ERROR_AMBIGUOUS_VARIANT_TYPE: Ambiguous variant type"
      case .exceptionTypeNotDefinedGlobally:
        return "ERROR_EXCEPTION_TYPE_NOT_DECLARED: Exception type is not defined globally"
      case .exceptionTypeNotDefined(let exprType):
        return "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: Exception type `\(exprType.description)` is not defined"
      case .ambiguousThrowType:
        return "ERROR_AMBIGUOUS_THROW_TYPE: Ambiguous throw type"
      case .ambiguousPanicType:
        return "ERROR_AMBIGUOUS_PANIC_TYPE: Ambiguous panic type"
      case .unexpectedLambda:
        return "ERROR_UNEXPECTED_LAMBDA: Unexpected lambda"
      case .unexpectedList:
        return "ERROR_UNEXPECTED_LIST: Unexpected list"
      case .tupleIndexOutOfBounds(index: let index):
        return "ERROR_TUPLE_INDEX_OUT_OF_BOUNDS: Unexpected access to component number \(index)"
      case .notATuple(let actualType):
        return "ERROR_NOT_A_TUPLE: Expected an expression of tuple type but got `\(actualType.description)`"
      case .unexpectedTuple:
        return "ERROR_UNEXPECTED_TUPLE: Expected an expression of a non-tuple type"
      case .unexpectedVariant:
        return "ERROR_UNEXPECTED_VARIANT: Expected an expression of a non-variant type"
      case .ambiguousReferenceType:
        return "ERROR_AMBIGUOUS_REFERENCE_TYPE: Ambiguous reference type"
      case .unexpectedMemoryAddress:
        return "ERROR_UNEXPECTED_MEMORY_ADDRESS: Unexpected memory address"
      case .notAReference(actualType: let actualType):
        return "ERROR_NOT_A_REFERENCE: Expected an expression of reference type but got `\(actualType.description)`"
      case .unexpectedFieldAccess(fieldName: let fieldName, recordType: let recordType):
        return "ERROR_UNEXPECTED_FIELD_ACCESS: unexpected access to field `\(fieldName)` in record of type `\(recordType.description)`"
      case .unexpectedInjection(expectedType: let expectedType):
        return "ERROR_UNEXPECTED_INJECTION: expected an expression of a non-sum type `\(expectedType.description)`"
      case .unexpectedTupleLength(expected: let expected, actual: let actual):
        return "ERROR_UNEXPECTED_TUPLE_LENGTH: Expected tuple of length \(expected), but actual length is \(actual)"
      case .ambiguousListType:
        return "ERROR_AMBIGUOUS_LIST_TYPE: Ambiguous list type"
      case .ambiguousSumType:
        return "ERROR_AMBIGUOUS_SUM_TYPE: type inference for sum types is not supported"
      case .unexpectedRecordType(expectedType: let expectedType):
        return "ERROR_UNEXPECTED_RECORD: unexpected record type, where expected type `\(expectedType)`"
    }
  }
}

extension TypecheckError: LocalizedError {
  var errorDescription: String? {
    switch self {
      case .missingMain:
        "ERROR_MISSING_MAIN No main function in program"
      case .requiredReturnType:
        "Return type is required"
      case .onlyOneArgument:
        "ERROR_INCORRECT_NUMBER_OF_ARGUMENTS: Exactly one argument expected in function declaration"
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

  try program.decls.forEach { try typecheck(decl: $0, context: context.copy()) }

  guard context.isMainPresent else {
    throw TypecheckError.missingMain
  }
}

func typecheck(decl: Decl, context: Context) throws {
  switch decl {
    case let .declFun(_, name, paramDecls, returnType, _, _, returnExpr):
      _print("decl function, name = \(name)")
      let newContext = context
        .copy()
        .add(paramDecls: paramDecls)

      let obtainedType = try typecheck(expr: returnExpr, expected: returnType, context: newContext)

      try assertEqual(expected: returnType, given: obtainedType)

    case let .declFunGeneric(_, name, _, _, _, _, _, _):
      _print("decl function generic, name = \(name)")
      assertionFailure("Not implemented")
      
    case let .declTypeAlias(name, _):
      _print("decl type alias, name = \(name)")
      assertionFailure("Not implemented")
      
    case .declExceptionType:
      _print("decl exception type")
      break

    case let .declExceptionVariant(name, _):
      _print("decl exception variant, name = \(name)")
      assertionFailure("Not implemented")
  }
}


func typecheck(expr: Expr, expected: StellaType?, context: Context) throws -> StellaType {
  switch expr {
    case .constTrue:
      return .bool

    case .constFalse:
      return .bool

    case .constUnit:
      return .unit

    case .constInt:
      return .nat

    case .constMemory(_):
      guard let expected else {
        throw TypecheckError.typeError(description: .ambiguousReferenceType)
      }
      guard case .ref(_) = expected else {
        throw TypecheckError.typeError(description: .unexpectedMemoryAddress)
      }

      return expected

    case .var(let name):
      return try context.get(by: name)

    case .panic:
      guard let expected else {
        throw TypecheckError.typeError(description: .ambiguousPanicType)
      }
      return expected

    case .throw(let expr):
      guard context.exceptionTypeDefined else  {
        throw TypecheckError.typeError(description: .exceptionTypeNotDefinedGlobally)
      }

      let exprType = try typecheck(expr: expr, expected: context.exceptionType, context: context.copy())

      guard exprType == context.exceptionType else {
        throw TypecheckError.typeError(description: .exceptionTypeNotDefined(exprType: exprType))
      }

      guard let expected else {
        throw TypecheckError.typeError(description: .ambiguousThrowType)
      }

      return expected

    case .tryCatch(let tryExpr, let pat, let fallbackExpr):
      let tryExprType = try typecheck(expr: tryExpr, expected: expected, context: context.copy())
      guard context.exceptionTypeDefined, let exceptionType = context.exceptionType else  {
        throw TypecheckError.typeError(description: .exceptionTypeNotDefinedGlobally)
      }

      let fallbackExprCtx = try checkPattern(pattern: pat, type: exceptionType, context: context.copy())
      let fallbackType = try typecheck(expr: fallbackExpr, expected: expected, context: fallbackExprCtx)

      if let expected {
        guard tryExprType == expected else {
          throw TypecheckError.typeError(description: .typeMismatch(expectedType: expected, givenType: tryExprType))
        }
        guard fallbackType == expected else {
          throw TypecheckError.typeError(description: .typeMismatch(expectedType: expected, givenType: fallbackType))
        }
        return expected
      }

      guard tryExprType == fallbackType else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: fallbackType, givenType: tryExprType))
      }
      return tryExprType

    case .tryWith(let tryExpr, let fallbackExpr):
      let tryExprType = try typecheck(expr: tryExpr, expected: expected, context: context.copy())
      let fallbackExprType = try typecheck(expr: fallbackExpr, expected: expected, context: context.copy())

      if let expected {
        guard tryExprType == expected else {
          throw TypecheckError.typeError(description: .typeMismatch(expectedType: expected, givenType: tryExprType))
        }
        guard fallbackExprType == expected else {
          throw TypecheckError.typeError(description: .typeMismatch(expectedType: expected, givenType: fallbackExprType))
        }
        return expected
      } else {
        guard tryExprType == fallbackExprType else {
          throw TypecheckError.typeError(description: .custom("ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: Try with tryExpr and fallbackExpr must have same type"))
        }
        return tryExprType
      }

    case .inl(let expr):
      guard let expected else {
        throw TypecheckError.typeError(description: .ambiguousSumType)
      }
      guard case .sum(let lhs, _) = expected else {
        throw TypecheckError.typeError(description: .unexpectedInjection(expectedType: expected))
      }
      let actualType = try typecheck(expr: expr, expected: lhs, context: context.copy())
      guard lhs == actualType else {
        throw TypeErrorDescription.typeMismatch(expectedType: lhs, givenType: actualType)
      }
      return expected

    case .inr(let expr):
      guard let expected else {
        throw TypecheckError.typeError(description: .ambiguousSumType)
      }
      guard case .sum(_, let rhs) = expected else {
        throw TypecheckError.typeError(description: .unexpectedInjection(expectedType: expected))
      }
      let actualType = try typecheck(expr: expr, expected: rhs, context: context.copy())
      guard rhs == actualType else {
        throw TypeErrorDescription.typeMismatch(expectedType: rhs, givenType: actualType)
      }
      return expected

    case .listCons(let head, let tail):
      if let expected, expected.isList == false {
        throw TypecheckError.typeError(description: .unexpectedList(expectedType: expected))
      }
      var exp: StellaType? = nil
      if let expected, case .list(let elementType) = expected {
        exp = elementType
      }
      let headType = try typecheck(expr: head, expected: exp, context: context.copy())
      let tailType = try typecheck(expr: tail, expected: expected, context: context.copy())
      guard case .list(let tailElementType) = tailType else {
        throw TypecheckError.typeError(description: .expectedList(actualType: tailType))
      }
      try assertEqual(expected: headType, given: tailElementType)

      return tailType

    case .listHead(let list):
      let listType = try typecheck(expr: list, expected: nil, context: context.copy())
      guard case .list(let type) = listType else {
        throw TypecheckError.typeError(description: .expectedList(actualType: listType))
      }

      try assertEqual(expected: expected, given: type)

      return type

    case .listIsEmpty(let list):
      let type = try typecheck(expr: list, expected: nil, context: context.copy())

      guard case .list(_) = type else {
        throw TypecheckError.typeError(description: .expectedList(actualType: type))
      }

      try assertEqual(expected: expected, given: .bool)

      return .bool

    case .listTail(let list):
      let type = try typecheck(expr: list, expected: nil, context: context.copy())

      guard case .list(_) = type else {
        throw TypecheckError.typeError(description: .expectedList(actualType: type))
      }

      try assertEqual(expected: expected, given: type)

      return type

    case .succ(let n):
      let type = try typecheck(expr: n, expected: .nat, context: context.copy())
      guard type == .nat else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: type))
      }
      return .nat

    case .logicNot(let expr):
      let type = try typecheck(expr: expr, expected: .bool, context: context.copy())

      guard type == .bool else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .bool, givenType: type))
      }

      return .bool

    case .natPred(let n):
      let type = try typecheck(expr: n, expected: .nat, context: context.copy())
      guard type == .nat else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: type))
      }
      return .nat

    case .natIsZero(let n):
      let type = try typecheck(expr: n, expected: .nat, context: context.copy())
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

      let typeN = try typecheck(expr: n, expected: .nat, context: context.copy())
      guard typeN == .nat else {
        throw TypecheckError.typeError(
          description: .custom("ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: First parameter in <Nat::rec> is expected to be Nat, got \(typeN.description)")
        )
      }

      let typeInitial = try typecheck(expr: initial, expected: nil, context: context.copy())

      let typeStep = try typecheck(
        expr: step,
        expected: .fun(
          parameterTypes: [.nat],
          returnType: .fun(
            parameterTypes: [typeInitial],
            returnType: typeInitial
          )
        ), // fn(Nat) -> (fn(T) -> T)
        context: context.copy()
      )
      guard
        case let .fun(parameterTypesStep, returnTypeStep) = typeStep,
        parameterTypesStep[0] == .nat
      else {
        throw TypecheckError.typeError(
          description: .custom("ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: Third parameter in <Nat::rec> is expected to be Fun(Nat) -> (Fun(T) -> T), got \(typeStep.description)")
        )
      }

      guard
        case let .fun(lambdaTypes, lambdaReturnType) = returnTypeStep,
        lambdaTypes[0] == typeInitial,
        lambdaReturnType == typeInitial
      else {
        throw TypecheckError.typeError(
          description: .custom("ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: Return type of the third parameter in <Nat::rec> is expected to be Fun(\(typeInitial.description)) -> \(typeInitial.description), got \(typeStep.description)")
        )
      }

      return typeInitial

    case .fix(let expr):
      let actualType = try typecheck(expr: expr, expected: expected, context: context.copy())
      guard case .fun(let parameterTypes, let returnType) = actualType else {
        throw TypecheckError.typeError(description: .notAFunction(actualType: actualType))
      }

      guard parameterTypes[0] == returnType else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: parameterTypes[0], givenType: returnType))
      }

      return returnType

    case .fold(_, _):
      assertionFailure("Not implemented")

    case .unfold(_, _):
      assertionFailure("Not implemented")

    case .application(let expr, let exprs):
      guard exprs.count == 1 else {
        throw TypecheckError.onlyOneArgument
      }

      let funcType = try typecheck(expr: expr, expected: nil, context: context.copy())
      guard case let .fun(parameterTypes, returnType) = funcType else {
        throw TypecheckError.typeError(
          description: .notAFunction(actualType: funcType)
        )
      }

      let argType = try typecheck(expr: exprs[0], expected: parameterTypes[0], context: context.copy())

      guard parameterTypes[0] == argType else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: parameterTypes[0], givenType: argType))
      }

      return returnType

    case .typeApplication(_, _):
      assertionFailure("Not implemented")

    case .multiply(let left, let right),
        .divide(let left, let right),
        .subtract(let left, let right),
        .add(let left, let right):
      let typeLeft = try typecheck(expr: left, expected: .nat, context: context.copy())
      let typeRight = try typecheck(expr: right, expected: .nat, context: context.copy())

      guard typeLeft == .nat else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: typeLeft))
      }
      guard typeRight == .nat  else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: typeRight))
      }

      return .nat

    case .logicAnd(let left, let right),
         .logicOr(let left, let right):
      let typeLeft = try typecheck(expr: left, expected: .bool, context: context.copy())
      let typeRight = try typecheck(expr: right, expected: .bool, context: context.copy())

      guard typeLeft == .bool else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .bool, givenType: typeLeft))
      }
      guard typeRight == .bool  else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .bool, givenType: typeRight))
      }

      return .bool

    case .ref(let expr):
      let type = try typecheck(expr: expr, expected: .nat, context: context.copy())

      return .ref(type: type)

    case .deref(let expr):
      let type = try typecheck(expr: expr, expected: .ref(type: .nat), context: context.copy())

      guard case let .ref(refType) = type else {
        throw TypecheckError.typeError(
          description: .notAReference(actualType: type)
        )
      }
      return refType

    case .typeAsc(let expr, let ascribedType):
      let typeExpr = try typecheck(expr: expr, expected: ascribedType, context: context.copy())
      guard typeExpr == ascribedType else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: ascribedType, givenType: typeExpr))
      }
      return ascribedType

    case .typeCast(_, _):
      assertionFailure("Not implemented")

    case .abstraction(let paramDecls, let returnExpr):
      var expectedReturnType: StellaType? = nil
      if let expected {
        guard case .fun(_, let expectedReturn) = expected else {
          throw TypecheckError.typeError(description: .unexpectedLambda)
        }
        expectedReturnType = expectedReturn
      }

      guard paramDecls.count == 1 else {
        throw TypecheckError.onlyOneArgument
      }

      let newContext = context
        .copy()
        .add(paramDecls: paramDecls)

      let exprResultType = try typecheck(expr: returnExpr, expected: expectedReturnType, context: newContext)

      return .fun(parameterTypes: paramDecls.map(\.type), returnType: exprResultType)

    case .tuple(let exprs):
      if let expected {
        guard case .tuple(let types) = expected else {
          throw TypecheckError.typeError(description: .unexpectedTuple)

        }
        guard types.count == exprs.count else {
          throw TypecheckError.typeError(description: .unexpectedTupleLength(expected: types.count, actual: exprs.count))
        }
        return try .tuple(
          types: zip(types, exprs).map {
            try typecheck(expr: $0.1, expected: $0.0, context: context.copy())
          }
        )
      }
      return try .tuple(types: exprs.map { try typecheck(expr: $0, expected: nil, context: context.copy()) })

    case .dotTuple(let expr, let index):
      let tupleType = try typecheck(expr: expr, expected: nil, context: context.copy())
      guard case let .tuple(types) = tupleType else {
          throw TypecheckError.typeError(
            description: .notATuple(actualType: tupleType)
          )
      }

      guard 0 < index, index <= types.count else {
          throw TypecheckError.typeError(
            description: .tupleIndexOutOfBounds(index: index)
          )
      }

      try assertEqual(expected: expected, given: types[index - 1])

      return types[index - 1]

    case .record(let bindings):
      if let expected {
        guard case .record(_) = expected else {
          throw TypecheckError.typeError(description: .unexpectedRecordType(expectedType: expected))
        }
      }
      let recordNames = bindings.map(\.name)
      guard recordNames.allElementsUnique == true else {
        throw TypecheckError.typeError(
          description: .custom("ERROR_DUPLICATE_RECORD_FIELDS: Duplicate record type fields")
        )
      }

      return .record(
          fieldTypes: try bindings.map { RecordFieldType(
              label: $0.name,
              type: try typecheck(expr: $0.rhs, expected: nil, context: context.copy())
          )}
      )

    case .dotRecord(let expr, let label):
      let recordType = try typecheck(expr: expr, expected: nil, context: context.copy())
      guard case let .record(fieldTypes) = recordType else {
          throw TypecheckError.typeError(
            description: .custom("ERROR_NOT_A_RECORD: DotRecord operator must be applied to record type, got \(recordType.description)")
          )
      }

      guard let fieldType = fieldTypes.first(where: { $0.label == label }) else {
          throw TypecheckError.typeError(
            description: .unexpectedFieldAccess(fieldName: label, recordType: recordType)
          )
      }

      try assertEqual(expected: expected, given: fieldType.type)

      return fieldType.type

    case .variant(let label, let expr):
      guard let expected else {
        throw TypecheckError.typeError(description: .ambiguousVariantType)
      }

      guard case .variant(let fieldTypes) = expected else {
        throw TypecheckError.typeError(description: .unexpectedVariant)
      }

      guard fieldTypes.count > 0 else {
        throw TypecheckError.typeError(description: .custom("ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: Expected variant type with at least one field"))
      }

      guard let field = fieldTypes.first(where: { $0.label == label }) else {
        throw TypecheckError.typeError(description: .unexpectedVariantLabel)
      }

      let fieldExpectedType = field.type

      let fieldType = try typecheck(expr: expr!, expected: fieldExpectedType, context: context.copy())

      if let fieldExpectedType {
        guard fieldType == fieldExpectedType  else {
          throw TypecheckError.typeError(description: .typeMismatch(expectedType: fieldExpectedType, givenType: fieldType))
        }
      }
      return expected

    case .match(let expr, let cases):
      let exprType = try typecheck(expr: expr, expected: nil, context: context.copy())
      guard cases.count != 0 else {
        throw TypecheckError.typeError(description: .illegalEmptyMatching)
      }
      var caseBodyExpectedType: StellaType? = expected

      for matchCase in cases {
        let contextCopy = context.copy()
        let extendedCtx = try checkPattern(pattern: matchCase.pattern, type: exprType, context: contextCopy)
        let inferredType = try typecheck(expr: matchCase.expr, expected: expected, context: extendedCtx)
        if let caseBodyExpectedType {
          guard caseBodyExpectedType == inferredType else {
            throw TypecheckError.typeError(description: .typeMismatch(expectedType: caseBodyExpectedType, givenType: inferredType))
          }
        } else {
          caseBodyExpectedType = inferredType
        }
      }
      guard let caseBodyExpectedType else {
        throw TypecheckError.typeError(
          description: .illegalEmptyMatching
        )
      }
      guard isExhaustive(exprType: exprType, casePatterns: cases.map(\.pattern)) else {
        throw TypecheckError.typeError(description: .nonExhaustiveMatchPatterns)
      }
      return caseBodyExpectedType

    case .list(let exprs):
      let listTypes = try exprs.map { try typecheck(expr: $0, expected: nil, context: context.copy()) }

      guard listTypes.allElementsEqual == true else {
        throw TypecheckError.typeError(description: .listContainsDifferentTypes)
      }
      
      if let expected {
        guard case .list(type: let expectedElementsType) = expected  else {
          throw TypecheckError.typeError(description: .unexpectedList(expectedType: expected))
        }

        if listTypes.count == 0 {
          return .list(type: expectedElementsType)
        }

        guard let actualElementsType = listTypes.first else {
          assertionFailure("Something wrong! Impossible")
          return .top
        }

        guard actualElementsType == expectedElementsType else {
          throw TypecheckError.typeError(description: .custom("ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: Expected elements of list is \(expectedElementsType), but actual type is \(actualElementsType)"))
        }
      }

      guard let first = listTypes.first else {
        throw TypecheckError.typeError(description: .ambiguousListType)
      }

      return .list(type: first)

    case .lessThan(let left, let right),
        .lessThanOrEqual(let left, let right),
        .greaterThanOrEqual(let left, let right),
        .greaterThan(let left, let right):
      let typeLeft = try typecheck(expr: left, expected: .nat, context: context.copy())
      let typeRight = try typecheck(expr: right, expected: .nat, context: context.copy())

      guard typeLeft == .nat else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: typeLeft))
      }
      guard typeRight == .nat  else {
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: .nat, givenType: typeRight))
      }

      return .bool

    case .notEqual(let left, let right),
         .equal(let left, let right):
      let leftType = try typecheck(expr: left, expected: nil, context: context.copy())
      let rightType = try typecheck(expr: right, expected: nil, context: context.copy())

      guard leftType == rightType else {
        throw TypecheckError.typeError(
          description: .custom("ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: Lhs and rhs of equal exp must be the same type, got \(leftType.description) and  \(rightType.description)")
        )
      }

      return .bool

    case .assign(let lhs, let rhs):
      let lhsType = try typecheck(expr: lhs, expected: nil, context: context.copy())
      let rhsType = try typecheck(expr: rhs, expected: nil, context: context.copy())

      guard case .ref(let refExprType) = lhsType else {
        throw TypecheckError.typeError(description: .notAReference(actualType: lhsType))
      }

      guard refExprType == rhsType else {
//        throw TypecheckError.typeError(description: .custom("Expected type ref to \(refExprType), but try to assign \(rhsType) value"))
        throw TypecheckError.typeError(description: .typeMismatch(expectedType: refExprType, givenType: rhsType))
      }

      return .unit

    case .if(let condition, let thenExpr, let elseExpr):
      let conditionType = try typecheck(expr: condition, expected: .bool, context: context.copy())

      guard conditionType == .bool else {
        throw TypecheckError.typeError(
          description: .custom("ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: If condition must have `Bool` type but \(conditionType.description) was provided")
        )
      }

      let thenExprType = try typecheck(expr: thenExpr, expected: expected, context: context.copy())
      let elseExprType = try typecheck(expr: elseExpr, expected: expected, context: context.copy())

      guard (thenExprType == elseExprType) || (elseExprType == thenExprType) else {
        throw TypecheckError.typeError(
          description: .custom("ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: If's then and else blocks must have the same type")
        )
      }

      return thenExprType

    case .let(let patternBindings, let body):
      guard let pattern = patternBindings.first,
            case .var(let name) = pattern.pat
      else {
        throw TypecheckError.notImplemented
      }

      let type = try typecheck(expr: pattern.rhs, expected: nil, context: context.copy())
      let newContext = context
        .add(name: name, type: type)

      return try typecheck(expr: body, expected: expected, context: newContext)

    case .letRec(_, _):
      assertionFailure("Not implemented")

    case .typeAbstraction(let generics, let expr):
      assertionFailure("Not implemented")

    case .sequence(let expr1, let expr2):
      // If we have only one expression, the type of the sequence is the type of this expression
      guard let expr2 else {
        return try typecheck(expr: expr1, expected: expected, context: context)
      }

      // If there are two expressions in the sequence, the first operand must have the type Unit and its value is discarded.
      let type1 = try typecheck(expr: expr1, expected: .unit, context: context)
      guard type1 == .unit else {
          throw TypecheckError.typeError(
            description: .illegalEmptyMatching
          )
      }

      return try typecheck(expr: expr2, expected: expected, context: context)
  }
  fatalError("Not implemented")
}


func checkPattern(pattern: Pattern, type: StellaType, context: Context) throws -> Context {
  switch pattern {
    case .var(let name):
      return context.add(name: name, type: type)
    case .inl(let pat):
      guard case .sum(let left, _) = type else {
        throw TypecheckError.typeError(description: .unexpectedPatternForType)
      }
      return try checkPattern(pattern: pat, type: left, context: context)
    case .inr(let pat):
      guard case .sum(_, let right) = type else {
        throw TypecheckError.typeError(description: .unexpectedPatternForType)
      }
      return try checkPattern(pattern: pat, type: right, context: context)
    case .variant(let label, let innerPattern):
      guard case .variant(let fieldTypes) = type else {
        throw TypecheckError.typeError(description: .unexpectedPatternForType)
      }
      guard let field = fieldTypes.first(where: { $0.label == label }) else {
        throw TypecheckError.typeError(description: .unexpectedPatternForType)
      }
      return try checkPattern(pattern: innerPattern!, type: field.type!, context: context.copy())
    case .int(_):
      return context
    default:
      fatalError("Not implemented")
  }
}
