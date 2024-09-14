//
//  typecheck.swift
//  
//
//  Created by Nikolai Kudasov on 27.03.2023.
//

public func typecheck(decl: Decl) {
  switch decl {
    case let .declFun(_, name, _, _, _, _, _):
      print("decl function, name = \(name)")
      
    case let .declFunGeneric(_, name, _, _, _, _, _, _):
      print("decl function generic, name = \(name)")
      
    case let .declTypeAlias(name, _):
      print("decl type alias, name = \(name)")
      
    case .declExceptionType:
      print("decl exception type")
      
    case let .declExceptionVariant(name, _):
      print("decl exception variant, name = \(name)")
  }
}

public func typecheck(program: Program) throws {
  program.decls.forEach { typecheck(decl: $0) }
}
