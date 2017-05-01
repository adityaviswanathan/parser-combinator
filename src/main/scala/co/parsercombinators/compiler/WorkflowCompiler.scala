package co.parsercombinators.compiler

import co.parsercombinators.lexer.WorkflowLexer
import co.parsercombinators.parser.{WorkflowParser, WorkflowAST}

object WorkflowCompiler {
  def apply(code: String): Either[WorkflowCompilationError, WorkflowAST] = {
    for {
      tokens <- WorkflowLexer(code).right
      ast <- WorkflowParser(tokens).right
    } yield ast
  }
}
