import Antlr4
import Foundation

@main
public struct stella_implementation_in_swift {
  
  public static func typecheck_file(filepath: String) throws {
    let lexer = try! stellaLexer(ANTLRInputStream(String(contentsOfFile: filepath)));

    let parser = try stellaParser(CommonTokenStream(lexer))
    let ctx = try parser.program()
    let program = try buildProgram(ctx: ctx)

    try typecheck(program: program)
  }
  
  public static func main() {
    assert(CommandLine.arguments[1] == "typecheck")

    do {
      try typecheck_file(filepath: CommandLine.arguments[2])
    } catch {
      print(error)
      print("Parse Error occurred. See the message above.")
    }
  }
}
