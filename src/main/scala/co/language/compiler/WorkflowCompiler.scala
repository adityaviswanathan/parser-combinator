package co.language.compiler

import co.language.lexer.WorkflowLexer
import co.language.parser.{WorkflowParser, WorkflowAST}
import co.language.parser._
import co.language.runtime.{WorkflowRuntime}
import scala.collection.mutable.{Map}

object WorkflowCompiler {
  def apply(code: String): Either[WorkflowError, Map[String, Value]] = {
    for {
      tokens <- WorkflowLexer(code).right
      ast <- WorkflowParser(tokens).right
      env <- WorkflowRuntime(ast).right
    } yield env
  }

  def syntax(code: String): Either[WorkflowError, WorkflowAST] = {
    for {
      tokens <- WorkflowLexer(code).right
      ast <- WorkflowParser(tokens).right
    } yield ast
  }
}
