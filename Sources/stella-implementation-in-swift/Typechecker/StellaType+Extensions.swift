//
//  StellaType+Extensions.swift
//  
//
//  Created by Антон Нехаев on 14.09.2024.
//

import Foundation

extension StellaType: Equatable {
  public static func == (lhs: StellaType, rhs: StellaType) -> Bool {
    switch (lhs, rhs) {
      case (_, .bot):
        return true

      case (.top, _):
        return true

      case (.bool, .bool),
        (.nat, .nat),
        (.unit, .unit):
        return true

      case let (.var(lhs), .var(rhs)):
        return lhs == rhs

      case let (.forAll(types1, type1), .forAll(types2, type2)):
        return Set(types1) == Set(types2) && type1 == type2

      case let (.fun(pt1, rt1), .fun(pt2, rt2)):
        return pt2[0] == pt1[0] && rt1 == rt2

      case let (.sum(lhs1, rhs1), .sum(lhs2, rhs2)):
        return lhs1 == lhs2 && rhs1 == rhs2

      case let (.ref(lhs), .ref(rhs)):
        return lhs == rhs

      case let (.tuple(lhs), .tuple(types: rhs)):
        return lhs == rhs

      case let (.record(lhs), .record(rhs)):
        let st1 = Set(lhs)
        let st2 = Set(rhs)
        return st1 == st2

      case let (.list(lhs), .list(rhs)):
        return lhs == rhs

      case let (.variant(lhs), .variant(rhs)):
        let st1 = Set(lhs)
        let st2 = Set(rhs)
//        return st1.isSubset(of: st2)
        return st1 == st2

      case (.undefined, .undefined):
        return true

      default:
        return false
    }
  }
}

extension StellaType: CustomStringConvertible {
  public var description: String {
    switch self {
      case let .fun(parameterTypes, returnType):
        return "fun(\(parameterTypes[0].description)) -> (\(returnType.description))"
      case let .forAll(types, type):
        return "forall \(String(types.reduce("(") { $0 + $1.description + ","}.dropLast()))).\(type.description)"
      case .bool:
        return "Bool"
      case .nat:
        return "Nat"
      case .unit:
        return "Unit"
      case let .sum(lhs, rhs):
        return "(\(lhs.description)) + (\(rhs.description))"
      case let .tuple(types):
        return String(types.reduce("{") { $0 + $1.description + ","}.dropLast()) + "}"
      case let .ref(type):
        return "Ref \(type.description)"
      case .top:
        return "Top"
      case .bot:
        return "Bot"
      case let .list(type):
        return "[\(type.description)]"
      case let .record(fieldTypes):
        return String(fieldTypes.reduce("{") { $0 + $1.label + " = " + $1.type.description + ","}.dropLast()) + "}"
      case .variant:
        return "Variant"
      case let .var(name):
        return "var(\(name))"
      case .undefined:
        return "No type"
    }
  }
}
