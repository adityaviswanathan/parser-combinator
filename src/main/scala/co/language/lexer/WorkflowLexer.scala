package co.language.lexer

import co.language.compiler.{Location, WorkflowLexerError}

import scala.util.parsing.combinator.RegexParsers

object WorkflowLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def apply(code: String): Either[WorkflowLexerError, List[WorkflowToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(WorkflowLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def tokens: Parser[List[WorkflowToken]] = {
    phrase(rep1(componentEnum | elementEnum | eventEnum | listenerEnum | connectiveEnum 
      | operatorEnum | argEnum | appEnum | exit | readInput | callService | switch 
      | otherwise | colon | arrow | equals | comma | literal | page | template | component 
      | event | listener | filter | connective | expression | arg | referenceArg | primitiveArg 
      | entity | property | identifier | indentation |  equal | openBlock | closeBlock 
      | openArray | closeArray )) ^^ { rawTokens =>
      processIndentations(rawTokens)
    }
  }

  private def processIndentations(tokens: List[WorkflowToken],
                                  indents: List[Int] = List(0)): List[WorkflowToken] = {
    tokens.headOption match {

      // if there is an increase in indentation level, we push this new level into the stack
      // and produce an INDENT
      case Some(INDENTATION(spaces)) if spaces > indents.head =>
        INDENT() :: processIndentations(tokens.tail, spaces :: indents)

      // if there is a decrease, we pop from the stack until we have matched the new level and
      // we produce a DEDENT for each pop
      case Some(INDENTATION(spaces)) if spaces < indents.head =>
        val (dropped, kept) = indents.partition(_ > spaces)
        (dropped map (_ => DEDENT())) ::: processIndentations(tokens.tail, kept)

      // if the indentation level stays unchanged, no tokens are produced
      case Some(INDENTATION(spaces)) if spaces == indents.head =>
        processIndentations(tokens.tail, indents)

      // other tokens are ignored
      case Some(token) =>
        token :: processIndentations(tokens.tail, indents)

      // the final step is to produce a DEDENT for each indentation level still remaining, thus
      // "closing" the remaining open INDENTS
      case None =>
        indents.filter(_ > 0).map(_ => DEDENT())

    }
  }

  def identifier: Parser[IDENTIFIER] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
  }

  def literal: Parser[LITERAL] = positioned {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      LITERAL(content)
    }
  }

  def indentation: Parser[INDENTATION] = positioned {
    "\n[ ]*".r ^^ { whitespace =>
      val nSpaces = whitespace.length - 1
      INDENTATION(nSpaces)
    }
  }

  def componentEnum: Parser[COMPONENTTYPE] = positioned {
    "ENUM.COMPONENT.[A-Z_][A-Z0-9_]*".r ^^ { str => 
      val keys = str.split('.')
      COMPONENTTYPE(keys(2))
    }
  }

  def elementEnum: Parser[ELEMENTTYPE] = positioned {
    "ENUM.ELEMENT.[A-Z_][A-Z0-9_]*".r ^^ { str => 
      val keys = str.split('.')
      ELEMENTTYPE(keys(2)) 
    }
  }

  def eventEnum: Parser[EVENTTYPE] = positioned {
    "ENUM.EVENT.[A-Z_][A-Z0-9_]*".r ^^ { str => 
      val keys = str.split('.')
      EVENTTYPE(keys(2)) 
    }
  }

  def listenerEnum: Parser[LISTENERTYPE] = positioned {
    "ENUM.LISTENER.[A-Z_][A-Z0-9_]*".r ^^ { str => 
      val keys = str.split('.')
      LISTENERTYPE(keys(2)) 
    }
  }

  def connectiveEnum: Parser[CONNECTIVETYPE] = positioned {
    "ENUM.CONNECTIVE.[A-Z_][A-Z0-9_]*".r ^^ { str => 
      val keys = str.split('.')
      CONNECTIVETYPE(keys(2)) 
    }
  }

  def operatorEnum: Parser[OPERATORTYPE] = positioned {
    "ENUM.OPERATOR.[A-Z_][A-Z0-9_]*".r ^^ { str => 
      val keys = str.split(".")
      OPERATORTYPE(keys(2)) 
    }
  }

  def argEnum: Parser[ARGTYPE] = positioned {
    "ENUM.ARG.[A-Z_][A-Z0-9_]*".r ^^ { str => 
      val keys = str.split(".")
      ARGTYPE(keys(2)) 
    }
  }

  def appEnum: Parser[APPTYPE] = positioned {
    "ENUM.APP.[A-Z_][A-Z0-9_]*".r ^^ { str => 
      val keys = str.split(".")
      APPTYPE(keys(2)) 
    }
  }

  def exit          = positioned { "exit"          ^^ (_ => EXIT()) }
  def readInput     = positioned { "read input"    ^^ (_ => READINPUT()) }
  def callService   = positioned { "call service"  ^^ (_ => CALLSERVICE()) }
  def switch        = positioned { "switch"        ^^ (_ => SWITCH()) }
  def otherwise     = positioned { "otherwise"     ^^ (_ => OTHERWISE()) }
  def colon         = positioned { ":"             ^^ (_ => COLON()) }
  def arrow         = positioned { "->"            ^^ (_ => ARROW()) }
  def equals        = positioned { "=="            ^^ (_ => EQUALS()) }
  def comma         = positioned { ","             ^^ (_ => COMMA()) }

  def equal         = positioned { "="             ^^ (_ => EQUAL()) }
  def openBlock     = positioned { "("             ^^ (_ => OPENBLOCK()) }
  def closeBlock    = positioned { ")"             ^^ (_ => CLOSEBLOCK()) }
  def openArray     = positioned { "["             ^^ (_ => OPENARRAY()) }
  def closeArray    = positioned { "]"             ^^ (_ => CLOSEARRAY()) }

  def page          = positioned { "PAGE"          ^^ (_ => PAGE()) }
  def template      = positioned { "TEMPLATE"      ^^ (_ => TEMPLATE()) }
  def component     = positioned { "COMPONENT"     ^^ (_ => COMPONENT()) }
  def event         = positioned { "EVENT"         ^^ (_ => EVENT()) }
  def listener      = positioned { "LISTENER"      ^^ (_ => LISTENER()) }
  def filter        = positioned { "FILTER"        ^^ (_ => FILTER()) }
  def connective    = positioned { "CONNECTIVE"    ^^ (_ => CONNECTIVE()) }
  def expression    = positioned { "EXPRESSION"    ^^ (_ => EXPRESSION()) }
  def arg           = positioned { "ARG"           ^^ (_ => ARG()) }
  def referenceArg  = positioned { "REFERENCEARG"  ^^ (_ => REFERENCEARG()) }
  def primitiveArg  = positioned { "PRIMITIVEARG"  ^^ (_ => PRIMITIVEARG()) }
  def entity        = positioned { "ENTITY"        ^^ (_ => ENTITY()) }
  def property      = positioned { "PROPERTY"      ^^ (_ => PROPERTY()) }

}