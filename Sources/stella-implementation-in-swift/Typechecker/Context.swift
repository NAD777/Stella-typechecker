//
//  Context.swift
//  
//
//  Created by Антон Нехаев on 14.09.2024.
//

import Foundation

typealias ContextEntriesName = String

enum ContextError: Error {
  case contextEntryAlreadyExist(name: String)
  case contextEntryDoesNotExist(name: String)
}

extension ContextError: LocalizedError {
  var errorDescription: String? {
    switch self {
      case .contextEntryAlreadyExist(let name):
        "Context entrie with name \(name) already exist"
      case .contextEntryDoesNotExist(let name):
        "Context entries with name \(name) does not exist"
    }
  }
}

struct Context {
  private var contextEntries: [ContextEntriesName: StellaType] = [:] // contains funcs and variables

  func add(name: ContextEntriesName, type: StellaType) throws -> Self {
    var newContext = self
    newContext.contextEntries[name] = type

    return newContext
  }

  func get(by name: ContextEntriesName) throws -> StellaType {
    guard let type = contextEntries[name] else {
      throw ContextError.contextEntryDoesNotExist(name: name)
    }

    return type
  }
}

extension Context {
  func add(paramDecls: [ParamDecl]) throws -> Self {
    var newContext = self

    try paramDecls.forEach { decl in
      newContext = try newContext.add(name: decl.name, type: decl.type)
    }

    return newContext
  }
}


extension Context {
  var isMainPresent: Bool {
    contextEntries["main"] != nil
  }

  func assertNotPresent(for name: ContextEntriesName) throws {
    guard contextEntries[name] == nil else {
      throw ContextError.contextEntryAlreadyExist(name: name)
    }
  }
}

