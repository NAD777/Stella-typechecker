//
//  Pattern+Extensions.swift
//
//
//  Created by Антон Нехаев on 16.09.2024.
//

import Foundation

extension Pattern {
  func identifier() throws -> ContextEntryName {
    switch self {
      case .inl(let pat),
          .inr(let pat):
        guard case let .var(name) = pat else {
          throw TypecheckError.typeError(
            description: .custom("Pattern error, expected var in inl/inr")
          )
        }

        return name
      case .var(let name):
        return name
      default:
        assertionFailure("Not implemented")
        throw TypecheckError.notImplemented
    }
  }
}
