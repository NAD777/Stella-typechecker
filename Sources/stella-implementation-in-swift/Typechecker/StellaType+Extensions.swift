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

      case let (.var(name1), .var(name2)):
        return name1 == name2

      case let (.forAll(types1, type1), .forAll(types2, type2)):
        return Set(types1) == Set(types2) && type1 == type2

      case let (.fun(pt1, rt1), .fun(pt2, rt2)):
        return pt2[0] == pt1[0] && rt1 == rt2

      case let (.sum(lhs1, rhs1), .sum(lhs2, rhs2)):
        return lhs1 == lhs2 && rhs1 == rhs2

      case let (.ref(lhs), .ref(rhs)):
        return lhs == rhs

      case let (.tuple(lhs), .tuple(types: rhs)):
        return lhs.count == rhs.count ? lhs.elementsEqual(rhs) : false

      case let (.record(lhs), .record(rhs)):
        let st1 = Set(lhs)
        let st2 = Set(rhs)
        return st1.isSubset(of: st2)

      default:
        return false
    }
  }
}
