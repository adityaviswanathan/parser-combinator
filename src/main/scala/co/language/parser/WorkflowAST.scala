package co.language.parser

import scala.util.parsing.input.Positional

sealed trait WorkflowAST extends Positional
case class AndThen(step1: WorkflowAST, step2: WorkflowAST) extends WorkflowAST
case class ReadInput(inputs: Seq[String]) extends WorkflowAST
case class CallService(serviceName: String) extends WorkflowAST
case class Choice(alternatives: Seq[ConditionThen]) extends WorkflowAST
case object Exit extends WorkflowAST

sealed trait ConditionThen extends Positional { def thenBlock: WorkflowAST }
case class IfThen(predicate: Condition, thenBlock: WorkflowAST) extends ConditionThen
case class OtherwiseThen(thenBlock: WorkflowAST) extends ConditionThen

sealed trait Condition extends Positional
case class Equals(factName: String, factValue: String) extends Condition

// BEGIN LANGUAGE DEFINITION
// case class Declarations(variables: Seq[WorkflowAST]) extends WorkflowAST

sealed trait Attribute extends Positional
case class AttributeToValue(attrName: String, attrValue: Value) extends Attribute
case class AttributeToList(attrName: String, attrValue: Seq[Value]) extends Attribute

case class Enum(enumType: String, value: String) extends WorkflowAST

sealed trait Declaration extends WorkflowAST
case class Variable(key: String, value: Value) extends Declaration

sealed trait Value extends Positional
case class StringValue(value: String) extends Value
case class EnumValue(value: Enum) extends Value
case class ConstructorValue(value: Constructor) extends Value
case class VariableValue(key: String) extends Value

// LANGUAGE KEYWORDS (CONSTRUCTORS)
sealed trait Constructor extends WorkflowAST
case class App(attributes: Seq[Attribute]) extends Constructor
case class Page(attributes: Seq[Attribute]) extends Constructor
case class Template(attributes: Seq[Attribute]) extends Constructor
case class Component(attributes: Seq[Attribute]) extends Constructor
case class Event(attributes: Seq[Attribute]) extends Constructor
case class Listener(attributes: Seq[Attribute]) extends Constructor
case class Filter(attributes: Seq[Attribute]) extends Constructor
case class Connective(attributes: Seq[Attribute]) extends Constructor
case class Expression(attributes: Seq[Attribute]) extends Constructor
case class Arg(attributes: Seq[Attribute]) extends Constructor
case class ReferenceArg(attributes: Seq[Attribute]) extends Constructor
case class PrimitiveArg(attributes: Seq[Attribute]) extends Constructor
case class Entity(attributes: Seq[Attribute]) extends Constructor
case class Property(attributes: Seq[Attribute]) extends Constructor
