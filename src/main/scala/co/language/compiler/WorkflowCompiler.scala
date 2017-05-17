package co.language.compiler

import co.language.lexer.WorkflowLexer
import co.language.parser.{WorkflowParser, WorkflowAST}

object WorkflowCompiler {
  def apply(code: String): Either[WorkflowError, WorkflowAST] = {
    for {
      tokens <- WorkflowLexer(code).right
      ast <- WorkflowParser(tokens).right
    } yield ast
  }
}
