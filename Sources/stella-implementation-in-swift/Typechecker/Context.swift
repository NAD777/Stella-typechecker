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
        "Context entry with name `\(name)` already exist"
      case .contextEntryDoesNotExist(let name):
        "ERROR_UNDEFINED_VARIABLE Context entry with name `\(name)` does not exist"
    }
  }
}

final class Context {
  private var contextEntries: [ContextEntryName: StellaType] = [:] // contains funcs and variables
  private(set) var exceptionType: StellaType?

  func add(name: ContextEntryName, type: StellaType) -> Self {
    self.contextEntries[name] = type

    return self
  }

  func get(by name: ContextEntryName) throws -> StellaType {
    guard let type = contextEntries[name] else {
      throw ContextError.contextEntryDoesNotExist(name: name)
    }

    return type
  }

  func copy() -> Context {
    let newContext = Context()
    newContext.contextEntries = contextEntries
    newContext.exceptionType = exceptionType

    return newContext
  }
}

extension Context {
  func add(paramDecls: [ParamDecl]) -> Self {
    paramDecls.forEach { decl in
      _ = self.add(name: decl.name, type: decl.type)
    }

    return self
  }

  // used for top level function
  func add(decls: [Decl]) throws -> Self {
    try decls.forEach { decl in
      switch decl {
        case .declFun(let annotations, let name, let paramDecls, let returnType, let throwTypes, let localDecls, let returnExpr):
          try self.assertNotPresent(for: name)
          let functionType = try typeForFunction(paramDecls: paramDecls, returnType: returnType)

          _ = self.add(name: name, type: functionType)

        case .declFunGeneric(let annotations, let name, let generics, let paramDecls, let returnType, let throwTypes, let localDecls, let returnExpr):
          assertionFailure("Not implemented")
        case .declTypeAlias(let name, let type):
          assertionFailure("Not implemented")
        case .declExceptionType(let exceptionType):
          self.exceptionType = exceptionType
        case .declExceptionVariant(let name, let variantType):
          assertionFailure("Not implemented")
      }
    }
    return self
  }
}


extension Context {
  var isMainPresent: Bool {
    contextEntries["main"] != nil
  }

  var exceptionTypeDefined: Bool {
    exceptionType != nil
  }

  func assertNotPresent(for name: ContextEntryName) throws {
    guard contextEntries[name] == nil else {
      throw ContextError.contextEntryAlreadyExist(name: name)
    }
  }
}

