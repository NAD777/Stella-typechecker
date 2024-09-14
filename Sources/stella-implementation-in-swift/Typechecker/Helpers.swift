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
