//
//  Context.swift
//  
//
//  Created by Антон Нехаев on 14.09.2024.
//

import Foundation

typealias ContextEntriesName = String

enum ContextError: Error {
  case ContextEntriesAlreadyExist
  case ContextEntriesDoesNotExist
}

struct Context {
  private var contextEntries: [ContextEntriesName: StellaType] // contains funcs and variables

  func add(name: ContextEntriesName, type: StellaType) throws -> Self {
    guard contextEntries[name] == nil else {
      throw ContextError.ContextEntriesAlreadyExist
    }

    var newContext = self
    newContext.contextEntries[name] = type

    return newContext
  }

  func get(by name: ContextEntriesName) throws -> StellaType {
    guard let type = contextEntries[name] else {
      throw ContextError.ContextEntriesDoesNotExist
    }

    return type
  }
}
