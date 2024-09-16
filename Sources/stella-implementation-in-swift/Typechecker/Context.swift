//
//  Context.swift
//  
//
//  Created by Антон Нехаев on 14.09.2024.
//

import Foundation

typealias ContextEntryName = String

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
  private var contextEntries: [ContextEntryName: StellaType] = [:] // contains funcs and variables

  func add(name: ContextEntryName, type: StellaType) throws -> Self {
    var newContext = self
    newContext.contextEntries[name] = type

    return newContext
  }

  func get(by name: ContextEntryName) throws -> StellaType {
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

  // used for top level function
  func add(decls: [Decl]) throws -> Self {
    var newContext = self

    try decls.forEach { decl in
      switch decl {
        case .declFun(let annotations, let name, let paramDecls, let returnType, let throwTypes, let localDecls, let returnExpr):
          try newContext.assertNotPresent(for: name)
          let functionType = try typeForFunction(paramDecls: paramDecls, returnType: returnType)
          newContext = try newContext
            .add(name: name, type: functionType)

        case .declFunGeneric(let annotations, let name, let generics, let paramDecls, let returnType, let throwTypes, let localDecls, let returnExpr):
          assertionFailure("Not implemented")
        case .declTypeAlias(let name, let type):
          assertionFailure("Not implemented")
        case .declExceptionType(let exceptionType):
          assertionFailure("Not implemented")
        case .declExceptionVariant(let name, let variantType):
          assertionFailure("Not implemented")
      }
    }
    return newContext
  }
}


extension Context {
  var isMainPresent: Bool {
    contextEntries["main"] != nil
  }

  func assertNotPresent(for name: ContextEntryName) throws {
    guard contextEntries[name] == nil else {
      throw ContextError.contextEntryAlreadyExist(name: name)
    }
  }
}

