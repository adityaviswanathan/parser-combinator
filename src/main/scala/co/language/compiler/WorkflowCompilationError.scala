package co.language.compiler

sealed trait WorkflowError

case class WorkflowLexerError(location: Location, msg: String) extends WorkflowError
case class WorkflowParserError(location: Location, msg: String) extends WorkflowError
case class WorkflowRuntimeError(location: Location, msg: String) extends WorkflowError

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}
