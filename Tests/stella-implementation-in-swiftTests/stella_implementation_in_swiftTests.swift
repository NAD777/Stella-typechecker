import XCTest
@testable import stella_implementation_in_swift

final class stella_implementation_in_swiftTests: XCTestCase {
  
  // MARK: - Tests
  
  func testCoreWellTyped() throws {
    let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
    let filepaths = filepaths(in: resourcePath + "/core/well-typed")
    checkWellTyped(filepaths: filepaths)
  }
  
  func testCoreIllTyped() throws {
    let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
    let filepaths = filepaths(in: resourcePath + "/core/ill-typed")
    checkIllTyped(filepaths: filepaths)
  }
  
  func testDevelopmentWellTyped() throws {
    let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
    let filepaths = filepaths(in: resourcePath + "/development/well-typed")
    checkWellTyped(filepaths: filepaths)
  }
  
  func testDevelopmentIllTyped() throws {
    let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
    let filepaths = filepaths(in: resourcePath + "/development/ill-typed")
    checkIllTyped(filepaths: filepaths)
  }

  // MARK: - Tuples

  func testTupleWellTyped() throws {
      let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
      let filepaths = filepaths(in: resourcePath + "/tuples/well-typed")
      checkWellTyped(filepaths: filepaths)
  }

  func testTupleIllTyped() throws {
      let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
      let filepaths = filepaths(in: resourcePath + "/tuples/ill-typed")
      checkIllTyped(filepaths: filepaths)
  }

  // MARK: - Let Recs

  func testLetRecsWellTyped() throws {
      let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
      let filepaths = filepaths(in: resourcePath + "/letrecs/well-typed")
      checkWellTyped(filepaths: filepaths)
  }

  func testLetRecsIllTyped() throws {
      let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
      let filepaths = filepaths(in: resourcePath + "/letrecs/ill-typed")
      checkIllTyped(filepaths: filepaths)
  }

  // MARK: - records

  func testRecordsWellTyped() throws {
      let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
      let filepaths = filepaths(in: resourcePath + "/records/well-typed")
      checkWellTyped(filepaths: filepaths)
  }

  func testRecordsIllTyped() throws {
      let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
      let filepaths = filepaths(in: resourcePath + "/records/ill-typed")
      checkIllTyped(filepaths: filepaths)
  }

  // MARK: - let bindings

  func testLetBindingsWellTyped() throws {
      let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
      let filepaths = filepaths(in: resourcePath + "/let-bindings/well-typed")
      checkWellTyped(filepaths: filepaths)
  }

  func testLetBindingsIllTyped() throws {
      let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
      let filepaths = filepaths(in: resourcePath + "/let-bindings/ill-typed")
      checkIllTyped(filepaths: filepaths)
  }

//  func testLetBindingsStructuralPatternsWellTyped() throws {
//      let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
//      let filepaths = filepaths(in: resourcePath + "/let-bindings/structural-patterns/well-typed")
//      checkWellTyped(filepaths: filepaths)
//  }
//
//  func testLetBindingsStructuralPatternsIllTyped() throws {
//      let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
//      let filepaths = filepaths(in: resourcePath + "/let-bindings/structural-patterns/ill-typed")
//      checkIllTyped(filepaths: filepaths)
//  }

 // MARK: - Pairs

 func testPairsWellTyped() throws {
     let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
     let filepaths = filepaths(in: resourcePath + "/pairs/well-typed")
     checkWellTyped(filepaths: filepaths)
 }

  func testPairsIllTyped() throws {
      let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
      let filepaths = filepaths(in: resourcePath + "/pairs/ill-typed")
      checkIllTyped(filepaths: filepaths)
  }

  // MARK: - ascriptions

  func testAscriptionsWellTyped() throws {
      let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
      let filepaths = filepaths(in: resourcePath + "/ascriptions/well-typed")
      checkWellTyped(filepaths: filepaths)
  }

   func testAscriptionsIllTyped() throws {
       let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
       let filepaths = filepaths(in: resourcePath + "/ascriptions/ill-typed")
       checkIllTyped(filepaths: filepaths)
   }

  // MARK: - sum types

  func testSumTypesWellTyped() throws {
      let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
      let filepaths = filepaths(in: resourcePath + "/sum-types/well-typed")
      checkWellTyped(filepaths: filepaths)
  }

   func testSumTypesIllTyped() throws {
       let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
       let filepaths = filepaths(in: resourcePath + "/sum-types/ill-typed")
       checkIllTyped(filepaths: filepaths)
   }

  // MARK: - lists

  func testListsWellTyped() throws {
      let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
      let filepaths = filepaths(in: resourcePath + "/lists/well-typed")
      checkWellTyped(filepaths: filepaths)
  }

   func testListsIllTyped() throws {
       let resourcePath = "\(Bundle.module.resourcePath!)/Resources"
       let filepaths = filepaths(in: resourcePath + "/lists/ill-typed")
       checkIllTyped(filepaths: filepaths)
   }

  // MARK: - Private Helpers
  
  private func checkWellTyped(filepaths: [String]) {
    for filepath in filepaths {
      print("Typechecking file <\(filepath.split(separator: "/").last!)>")
      XCTAssertNoThrow(try stella_implementation_in_swift.typecheck_file(filepath: filepath))
      print()
    }
  }
  
  private func checkIllTyped(filepaths: [String]) {
    for filepath in filepaths {
      print("Typechecking file <\(filepath.split(separator: "/").last!)>")
      XCTAssertThrowsError(
        try stella_implementation_in_swift.typecheck_file(filepath: filepath)
      ) { error in
        print(error.localizedDescription)
      }
      print()
    }
  }
  
  // MARK: - Private Helpers
  
  private func filepaths(in path: String) -> [String] {
    let contents = try! FileManager.default.contentsOfDirectory(
      at: URL(string: path)!,
      includingPropertiesForKeys: nil
    )
    
    return contents.filter { $0.hasDirectoryPath == false }.map { $0.path }
  }
  
}
