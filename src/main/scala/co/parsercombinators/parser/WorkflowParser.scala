package co.parsercombinators.parser

import co.parsercombinators.compiler.{Location, WorkflowParserError}
import co.parsercombinators.lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object WorkflowParser extends Parsers {
  override type Elem = WorkflowToken

  class WorkflowTokenReader(tokens: Seq[WorkflowToken]) extends Reader[WorkflowToken] {
    override def first: WorkflowToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[WorkflowToken] = new WorkflowTokenReader(tokens.tail)
  }


  def apply(tokens: Seq[WorkflowToken]): Either[WorkflowParserError, WorkflowAST] = {
    val reader = new WorkflowTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(WorkflowParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def program: Parser[WorkflowAST] = positioned {
    phrase(block)
  }

  def block: Parser[WorkflowAST] = positioned {
    rep1(statement) ^^ { case stmtList => stmtList reduceRight AndThen }
  }

  def statement: Parser[WorkflowAST] = positioned {
    val exit = EXIT() ^^ (_ => Exit)
    val readInput = READINPUT() ~ rep(identifier ~ COMMA()) ~ identifier ^^ {
      case read ~ inputs ~ IDENTIFIER(lastInput) => ReadInput(inputs.map(_._1.str) ++ List(lastInput))
    }
    val callService = CALLSERVICE() ~ literal ^^ {
      case call ~ LITERAL(serviceName) => CallService(serviceName)
    }
    val switch = SWITCH() ~ COLON() ~ INDENT() ~ rep1(ifThen) ~ opt(otherwiseThen) ~ DEDENT() ^^ {
      case _ ~ _ ~ _ ~ ifs ~ otherwise ~ _ => Choice(ifs ++ otherwise)
    }
    // val declareProperty = 
    //   (identifier ~ EQUAL() ~ PROPERTY() ~ OPENBLOCK() ~ INDENT() ~ 
    //   rep(attribute ~ COMMA()) ~ attribute ~ DEDENT() ~ CLOSEBLOCK()) ^^ {
    //     case IDENTIFIER(name) ~ _ ~ _ ~ _ ~ _ ~ attrs ~ lastAttribute ~ _ ~ _ => Property(name, attrs.map(_._1) ++ List(lastAttribute)) 
    // }

    val declare = (identifier ~ EQUAL() ~ value) ^^ { 
      case IDENTIFIER(id) ~ eq ~ value => Variable(id, value) 
    }

    declare | exit | readInput | callService | switch
  }

  def ifThen: Parser[IfThen] = positioned {
    (condition ~ ARROW() ~ INDENT() ~ block ~ DEDENT()) ^^ {
      case cond ~ _ ~ _ ~ block ~ _ => IfThen(cond, block)
    }
  }

  def otherwiseThen: Parser[OtherwiseThen] = positioned {
    (OTHERWISE() ~ ARROW() ~ INDENT() ~ block ~ DEDENT()) ^^ {
      case _ ~ _ ~ _ ~ block ~ _ => OtherwiseThen(block)
    }
  }

  def condition: Parser[Equals] = positioned {
    (identifier ~ EQUALS() ~ literal) ^^ { case IDENTIFIER(id) ~ eq ~ LITERAL(lit) => Equals(id, lit) }
  }

  def value: Parser[Value] = positioned {
    val stringValue = (literal) ^^ { case LITERAL(stringValue) => StringValue(stringValue) }
    val enumValue = (enum) ^^ { case e => EnumValue(e) }
    val constructorValue = (constructor) ^^ { case a => ConstructorValue(a) }
    val variableValue = (identifier) ^^ { case IDENTIFIER(id) => VariableValue(id) }

    stringValue | enumValue | constructorValue | variableValue
  }

  def enum: Parser[Enum] = positioned {
    val component = (componentEnum) ^^ { 
      case COMPONENTTYPE(value) => Enum("ComponentType", value) 
    }
    
    val element = (elementEnum) ^^ {
      case ELEMENTTYPE(value) => Enum("ElementType", value) 
    }

    val event = (eventEnum) ^^ {
      case EVENTTYPE(value) => Enum("EventType", value) 
    }

    val listener = (listenerEnum) ^^ {
      case LISTENERTYPE(value) => Enum("ListenerType", value) 
    }

    val connective = (connectiveEnum) ^^ {
      case CONNECTIVETYPE(value) => Enum("ConnectiveType", value) 
    }

    val operator = (operatorEnum) ^^ {
      case OPERATORTYPE(value) => Enum("OperatorType", value)
    }

    val arg = (argEnum) ^^ {
      case ARGTYPE(value) => Enum("ArgType", value)
    }

    val app = (appEnum) ^^ {
      case APPTYPE(value) => Enum("AppType", value)
    }


    component | element | event | listener | connective | operator | arg | app
  }

  def constructor: Parser[Constructor] = positioned {
    val pageConstructor = (PAGE() ~ OPENBLOCK() ~ INDENT() ~ 
    rep(attribute ~ COMMA()) ~ attribute ~ DEDENT() ~ CLOSEBLOCK()) ^^ {
     case _ ~ _ ~ _ ~ attrs ~ lastAttribute ~ _ ~ _ => Page(attrs.map(_._1) ++ List(lastAttribute)) }

    val templateConstructor = (TEMPLATE() ~ OPENBLOCK() ~ INDENT() ~ 
    rep(attribute ~ COMMA()) ~ attribute ~ DEDENT() ~ CLOSEBLOCK()) ^^ {
     case _ ~ _ ~ _ ~ attrs ~ lastAttribute ~ _ ~ _ => Template(attrs.map(_._1) ++ List(lastAttribute)) }

    val componentConstructor = (COMPONENT() ~ OPENBLOCK() ~ INDENT() ~ 
    rep(attribute ~ COMMA()) ~ attribute ~ DEDENT() ~ CLOSEBLOCK()) ^^ {
     case _ ~ _ ~ _ ~ attrs ~ lastAttribute ~ _ ~ _ => Component(attrs.map(_._1) ++ List(lastAttribute)) }

    val eventConstructor = (EVENT() ~ OPENBLOCK() ~ INDENT() ~ 
    rep(attribute ~ COMMA()) ~ attribute ~ DEDENT() ~ CLOSEBLOCK()) ^^ {
     case _ ~ _ ~ _ ~ attrs ~ lastAttribute ~ _ ~ _ => Event(attrs.map(_._1) ++ List(lastAttribute)) }

    val listenerConstructor = (LISTENER() ~ OPENBLOCK() ~ INDENT() ~ 
    rep(attribute ~ COMMA()) ~ attribute ~ DEDENT() ~ CLOSEBLOCK()) ^^ {
     case _ ~ _ ~ _ ~ attrs ~ lastAttribute ~ _ ~ _ => Listener(attrs.map(_._1) ++ List(lastAttribute)) }

    val filterConstructor = (FILTER() ~ OPENBLOCK() ~ INDENT() ~ 
    rep(attribute ~ COMMA()) ~ attribute ~ DEDENT() ~ CLOSEBLOCK()) ^^ {
     case _ ~ _ ~ _ ~ attrs ~ lastAttribute ~ _ ~ _ => Filter(attrs.map(_._1) ++ List(lastAttribute)) }

    val connectiveConstructor = (CONNECTIVE() ~ OPENBLOCK() ~ INDENT() ~ 
    rep(attribute ~ COMMA()) ~ attribute ~ DEDENT() ~ CLOSEBLOCK()) ^^ {
     case _ ~ _ ~ _ ~ attrs ~ lastAttribute ~ _ ~ _ => Connective(attrs.map(_._1) ++ List(lastAttribute)) }

    val expressionConstructor = (EXPRESSION() ~ OPENBLOCK() ~ INDENT() ~ 
    rep(attribute ~ COMMA()) ~ attribute ~ DEDENT() ~ CLOSEBLOCK()) ^^ {
     case _ ~ _ ~ _ ~ attrs ~ lastAttribute ~ _ ~ _ => Expression(attrs.map(_._1) ++ List(lastAttribute)) }

    val argConstructor = (ARG() ~ OPENBLOCK() ~ INDENT() ~ 
    rep(attribute ~ COMMA()) ~ attribute ~ DEDENT() ~ CLOSEBLOCK()) ^^ {
     case _ ~ _ ~ _ ~ attrs ~ lastAttribute ~ _ ~ _ => Arg(attrs.map(_._1) ++ List(lastAttribute)) }

    val referenceArgConstructor = (REFERENCEARG() ~ OPENBLOCK() ~ INDENT() ~ 
    rep(attribute ~ COMMA()) ~ attribute ~ DEDENT() ~ CLOSEBLOCK()) ^^ {
     case _ ~ _ ~ _ ~ attrs ~ lastAttribute ~ _ ~ _ => ReferenceArg(attrs.map(_._1) ++ List(lastAttribute)) }

    val primitiveArgConstructor = (PRIMITIVEARG() ~ OPENBLOCK() ~ INDENT() ~ 
    rep(attribute ~ COMMA()) ~ attribute ~ DEDENT() ~ CLOSEBLOCK()) ^^ {
     case _ ~ _ ~ _ ~ attrs ~ lastAttribute ~ _ ~ _ => PrimitiveArg(attrs.map(_._1) ++ List(lastAttribute)) }

    val entityConstructor = (ENTITY() ~ OPENBLOCK() ~ INDENT() ~ 
    rep(attribute ~ COMMA()) ~ attribute ~ DEDENT() ~ CLOSEBLOCK()) ^^ {
     case _ ~ _ ~ _ ~ attrs ~ lastAttribute ~ _ ~ _ => Entity(attrs.map(_._1) ++ List(lastAttribute)) }

    val propertyConstructor = (PROPERTY() ~ OPENBLOCK() ~ INDENT() ~ 
    rep(attribute ~ COMMA()) ~ attribute ~ DEDENT() ~ CLOSEBLOCK()) ^^ {
     case _ ~ _ ~ _ ~ attrs ~ lastAttribute ~ _ ~ _ => Property(attrs.map(_._1) ++ List(lastAttribute)) }
  
    (pageConstructor | templateConstructor | componentConstructor | eventConstructor | listenerConstructor 
      | filterConstructor | connectiveConstructor | expressionConstructor | argConstructor | referenceArgConstructor 
      | primitiveArgConstructor | entityConstructor | propertyConstructor)
  }

  def attribute: Parser[Attribute] = positioned {
    val attributeToList = (identifier ~ COLON() ~ OPENARRAY() ~ rep(value ~ COMMA()) ~ value ~ CLOSEARRAY()) ^^ {
      case IDENTIFIER(id) ~ _ ~ _ ~ vals ~ lastVal ~ _ => AttributeToList(id, vals.map(_._1) ++ List(lastVal))
    }

    val attributeToValue = (identifier ~ COLON() ~ value) ^^ {
      case IDENTIFIER(id) ~ _ ~ value => AttributeToValue(id, value)
    }

    attributeToList | attributeToValue
  } 

  private def identifier: Parser[IDENTIFIER] = positioned {
    accept("identifier", { case id @ IDENTIFIER(name) => id })
  }

  private def literal: Parser[LITERAL] = positioned {
    accept("string literal", { case lit @ LITERAL(name) => lit })
  }

  private def componentEnum: Parser[COMPONENTTYPE] = positioned {
    accept("component enum", { case componentValue @ COMPONENTTYPE(value) => componentValue })
  }

  private def elementEnum: Parser[ELEMENTTYPE] = positioned {
    accept("element enum", { case elementValue @ ELEMENTTYPE(value) => elementValue })
  }

  private def eventEnum: Parser[EVENTTYPE] = positioned {
    accept("event enum", { case eventValue @ EVENTTYPE(value) => eventValue })
  }

  private def listenerEnum: Parser[LISTENERTYPE] = positioned {
    accept("listener enum", { case listenerValue @ LISTENERTYPE(value) => listenerValue })
  }

  private def connectiveEnum: Parser[CONNECTIVETYPE] = positioned {
    accept("connective enum", { case connectiveValue @ CONNECTIVETYPE(value) => connectiveValue })
  }

  private def operatorEnum: Parser[OPERATORTYPE] = positioned {
    accept("operator enum", { case operatorValue @ OPERATORTYPE(value) => operatorValue })
  }

  private def argEnum: Parser[ARGTYPE] = positioned {
    accept("arg enum", { case argValue @ ARGTYPE(value) => argValue })
  }

  private def appEnum: Parser[APPTYPE] = positioned {
    accept("app enum", { case appValue @ APPTYPE(value) => appValue })
  }

}