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

func isExhaustive(exprType: StellaType, casePatterns: [Pattern]) -> Bool {
  // Convert the list of patterns to their respective types for comparison.
  let types = casePatterns.map { pattern -> String in
    switch pattern {
      case .variant(let label, _): return "PatternVariant:\(label)"
      case .inl: return "PatternInl"
      case .inr: return "PatternInr"
      case .`var`(_): return "PatternVar"
      default: return "OtherPattern"
    }
  }

  // If any pattern is a variable (PatternVar), the pattern matching is exhaustive.
  if types.contains("PatternVar") {
    return true
  }

  // Check the type of exprType.
  switch exprType {
    case .sum:
      // For sum types, both inl and inr patterns must be present.
      return types.contains("PatternInl") && types.contains("PatternInr")

    case .variant(let fieldTypes):
      // Identifying the used pattern labels for variant types.
      let usedPatternLabels: Set<String> = Set(casePatterns.compactMap { pattern in
        if case let .variant(label, _) = pattern {
          return label
        }
        return nil
      })

      // Variant type requires all field labels to be used in patterns.
      for fieldType in fieldTypes {
        if !usedPatternLabels.contains(fieldType.label) {
          return false
        }
      }
      return true

    default:
      // For any other type, return false, as exhaustive check is not defined.
      return false
  }
}
