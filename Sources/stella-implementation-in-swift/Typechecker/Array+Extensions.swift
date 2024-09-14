//
//  Array+Extensions.swift
//
//
//  Created by Антон Нехаев on 14.09.2024.
//

import Foundation

extension Array where Element: Equatable {
  var allElementsEqual: Bool {
    guard let firstElement = self.first else {
      return true
    }
    
    return self.dropFirst().allSatisfy { $0 == firstElement }
  }
}
