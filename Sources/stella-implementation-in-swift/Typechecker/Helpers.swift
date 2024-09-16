//
//  Helpers.swift
//
//
//  Created by Антон Нехаев on 14.09.2024.
//

import Foundation

func typeForFunction(paramDecls: [ParamDecl], returnType: StellaType?) throws -> StellaType {
  guard let returnType = returnType else {
    throw TypecheckError.requiredReturnType
  }
  
  guard paramDecls.count == 1 else {
    throw TypecheckError.onlyOneArgument
  }
  
  return .fun(parameterTypes: paramDecls.map { $0.type }, returnType: returnType)
}

func assertEqual(expected: StellaType?, given: StellaType) throws {
  guard let expected else { return }

  guard expected == given else {
    throw TypecheckError.typeError(description: .typeMismatch(expectedType: expected, givenType: given))
  }
}


func mergeTypes(_ type1: StellaType, _ type2: StellaType) throws -> StellaType {
  guard
    case let .sum(lhs1, rhs1) = type1,
    case let .sum(lhs2, rhs2) = type2
  else {
    return type1
  }

  if lhs1 == .undefined {
    return try .sum(left: lhs2, right: mergeTypes(rhs1, rhs2))
  }

  if rhs1 == .undefined {
    return try .sum(left: mergeTypes(lhs1, lhs2), right: rhs2)
  }

  if lhs2 == .undefined {
    return try .sum(left: lhs1, right: mergeTypes(rhs1, rhs2))
  }

  if rhs2 == .undefined {
    return try .sum(left: mergeTypes(lhs1, lhs2), right: rhs1)
  }

  throw TypecheckError.notSupported("Can't merge types \(type1.description), \(type2.description)")
}
