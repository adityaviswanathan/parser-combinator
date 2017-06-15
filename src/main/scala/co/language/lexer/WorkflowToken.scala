package co.language.lexer

import scala.util.parsing.input.Positional

sealed trait WorkflowToken extends Positional

case class IDENTIFIER(str: String) extends WorkflowToken
case class LITERAL(str: String) extends WorkflowToken
case class INDENTATION(spaces: Int) extends WorkflowToken
case class EXIT() extends WorkflowToken
case class READINPUT() extends WorkflowToken
case class CALLSERVICE() extends WorkflowToken
case class SWITCH() extends WorkflowToken
case class OTHERWISE() extends WorkflowToken
case class COLON() extends WorkflowToken
case class ARROW() extends WorkflowToken
case class EQUALS() extends WorkflowToken
case class COMMA() extends WorkflowToken
case class INDENT() extends WorkflowToken
case class DEDENT() extends WorkflowToken

// case class IDENTIFIER(str: String) extends WorkflowToken
// case class LITERAL(str: String) extends WorkflowToken
// case class INDENTATION(spaces: Int) extends WorkflowToken
case class OPENBLOCK() extends WorkflowToken
case class CLOSEBLOCK() extends WorkflowToken
case class OPENARRAY() extends WorkflowToken
case class CLOSEARRAY() extends WorkflowToken
// case class COMMA() extends WorkflowToken
// case class INDENT() extends WorkflowToken
// case class DEDENT() extends WorkflowToken

// VARIABLE ASSIGNMENT
case class EQUAL() extends WorkflowToken

// LANGUAGE KEYWORDS (CONSTRUCTORS)
case class CONSTRUCTOR() extends WorkflowToken
case class APP() extends WorkflowToken
case class PAGE() extends WorkflowToken
case class TEMPLATE() extends WorkflowToken
case class COMPONENT() extends WorkflowToken
case class EVENT() extends WorkflowToken
case class LISTENER() extends WorkflowToken
case class FILTER() extends WorkflowToken
case class CONNECTIVE() extends WorkflowToken
case class EXPRESSION() extends WorkflowToken
case class ARG() extends WorkflowToken
case class REFERENCEARG() extends WorkflowToken
case class PRIMITIVEARG() extends WorkflowToken
case class ENTITY() extends WorkflowToken
case class PROPERTY() extends WorkflowToken

// LANGUAGE ENUMS
// case class ENUM(enumType: String, value: String) extends WorkflowToken
case class COMPONENTTYPE(value: String) extends WorkflowToken
case class ELEMENTTYPE(value: String) extends WorkflowToken
case class EVENTTYPE(value: String) extends WorkflowToken
case class LISTENERTYPE(value: String) extends WorkflowToken
case class CONNECTIVETYPE(value: String) extends WorkflowToken
case class OPERATORTYPE(value: String) extends WorkflowToken
case class ARGTYPE(value: String) extends WorkflowToken
case class APPTYPE(value: String) extends WorkflowToken