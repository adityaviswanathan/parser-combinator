package co.parsercombinators

import co.parsercombinators.compiler.{Location, WorkflowCompiler, WorkflowParserError}
import co.parsercombinators.parser._
import org.scalatest.{FlatSpec, Matchers}
import co.parsercombinators.runtime.WorkflowRuntime

class WorkflowCompilerSpec extends FlatSpec with Matchers {

  val simpleDeclare = 
    """
      |dummy = "newString"
    """.stripMargin.trim

  val multiSimpleDeclare = 
    """
      |dummy = "newString"
      |dummy2 = dummy
      |dummy3 = ENUM.COMPONENT.STATIC
    """.stripMargin.trim

  val multiConstructorDeclare = 
    """
      |dummy = PROPERTY(
      |  Name : "NewProperty",
      |  Enum : ENUM.ELEMENT.HEADER
      |)
      |dummy2 = ENTITY(
      |  Props : [dummy, PROPERTY(
      |    Name : "SecondProperty",
      |    Enum : ENUM.ELEMENT.INPUT
      |  )],
      |  Name : "NewEntity" 
      |)
    """.stripMargin.trim

  val multiTypeDeclare = 
    """
      |dummy = PROPERTY(
      |  Name : "NewProperty",
      |  Enum : ENUM.COMPONENT.STATIC,
      |  Var : SomeVariable
      |)
    """.stripMargin.trim

  val nestedDeclare = 
    """
      |dummy = PROPERTY(
      |  Name : "NewProperty",
      |  Enum : ENUM.COMPONENT.STATIC,
      |  Var : PROPERTY(
      |    Name : "NestedProperty",
      |    Enum : ENUM.COMPONENT.STATIC
      |  )
      |)
    """.stripMargin.trim

  val doublyNestedDeclare = 
    """
      |dummy = PROPERTY(
      |  Name : "NewProperty",
      |  Enum : ENUM.COMPONENT.STATIC,
      |  Var : PROPERTY(
      |    Name : "NestedProperty",
      |    Enum : ENUM.COMPONENT.STATIC,
      |    Var : PROPERTY(
      |      Name : "DoublyNestedProperty",
      |      Enum : ENUM.COMPONENT.STATIC
      |    )
      |  )
      |)
    """.stripMargin.trim

  val simpleRuntime = 
    """
      |dummy = "newString"
      |dummy = "tester"
      |dummy2 = dummy
      |dummy3 = ENUM.COMPONENT.STATIC
      |dummy4 = PROPERTY( 
      |  Name : dummy2,
      |  Entity : ENUM.COMPONENT.STATIC
      |)
      |dummy2 = dummy4
    """.stripMargin.trim

  val validCode =
    """
      |read input name, country, face
      |switch:
      |  country == "PT" ->
      |    call service "A"
      |    exit
      |  otherwise ->
      |    call service "B"
      |    switch:
      |      name == "unknown" ->
      |        exit
      |      otherwise ->
      |        call service "C"
      |        exit
    """.stripMargin.trim

  val invalidCode =
    """
      |read input name, country
      |switch:
      |  country == PT ->
      |    call service "A"
      |    exit
      |  otherwise ->
      |    call service "B"
      |    switch:
      |      name == "unknown" ->
      |        exit
      |      otherwise ->
      |        call service "C"
      |        exit
    """.stripMargin.trim

  val successfulAST = AndThen(
    ReadInput(List("name", "country", "face")),
    Choice(List(
      IfThen( Equals("country", "PT"), AndThen(CallService("A"), Exit) ),
      OtherwiseThen(
        AndThen(
          CallService("B"),
          Choice(List(
            IfThen( Equals("name", "unknown"), Exit ),
            OtherwiseThen( AndThen(CallService("C"), Exit) )
          ))
        )
      )
    ))
  )

  val errorMsg = WorkflowParserError(Location(3,14), "string literal expected")

  val simpleAST = Variable("dummy", StringValue("newString"))
  val multiSimpleAST = AndThen(Variable("dummy", StringValue("newString")), AndThen(
    Variable("dummy2", VariableValue("dummy")),
    Variable("dummy3", EnumValue(Enum("ComponentType", "STATIC")))
  ))
  val multiConstructorAST = AndThen(Variable("dummy", ConstructorValue(
    Property(
      List(
        AttributeToValue("Name", StringValue("NewProperty")),
        AttributeToValue("Enum", EnumValue(Enum("ElementType", "HEADER")))
      )
    )
  )), Variable("dummy2", ConstructorValue(
    Entity(
      List(
        AttributeToList("Props", List(
          VariableValue("dummy"),
          ConstructorValue(Property(List(
              AttributeToValue("Name", StringValue("SecondProperty")),
              AttributeToValue("Enum", EnumValue(Enum("ElementType", "INPUT")))
          )))
        )),
        AttributeToValue("Name", StringValue("NewEntity"))
      )
    )
  )))
  val multiTypeAST = Variable("dummy", ConstructorValue(
    Property(
      List(
        AttributeToValue("Name", StringValue("NewProperty")),
        AttributeToValue("Enum", EnumValue(Enum("ComponentType", "STATIC"))),
        AttributeToValue("Var", VariableValue("SomeVariable"))
      )
    )
  ))
  val nestedAST = Variable("dummy", ConstructorValue(
    Property(
      List(
        AttributeToValue("Name", StringValue("NewProperty")),
        AttributeToValue("Enum", EnumValue(Enum("ComponentType", "STATIC"))),
        AttributeToValue("Var", ConstructorValue(Property(
          List(
            AttributeToValue("Name", StringValue("NestedProperty")),
            AttributeToValue("Enum", EnumValue(Enum("ComponentType", "STATIC")))
          )
        )))
      )
    )
  ))
  val doublyNestedAST = Variable("dummy", ConstructorValue(
    Property(
      List(
        AttributeToValue("Name", StringValue("NewProperty")),
        AttributeToValue("Enum", EnumValue(Enum("ComponentType", "STATIC"))),
        AttributeToValue("Var", ConstructorValue(Property(
          List(
            AttributeToValue("Name", StringValue("NestedProperty")),
            AttributeToValue("Enum", EnumValue(Enum("ComponentType", "STATIC"))),
            AttributeToValue("Var", ConstructorValue(Property(
              List(
                AttributeToValue("Name", StringValue("DoublyNestedProperty")),
                AttributeToValue("Enum", EnumValue(Enum("ComponentType", "STATIC")))
              )
            )))
          )
        )))
      )
    )
  ))
  val simpleRuntimeMap = Map(
    "dummy4" -> ConstructorValue(Property(
      List(
        AttributeToValue("Name",VariableValue("dummy2")), 
        AttributeToValue("Entity",EnumValue(Enum("ComponentType", "STATIC")))
      ))), 
    "dummy" -> StringValue("tester"), 
    "dummy3" -> EnumValue(Enum("ComponentType","STATIC")), 
    "dummy2" -> VariableValue("dummy4")
  )

  "Workflow compiler" should "successfully parse a simple variable declaration" in {
    WorkflowCompiler(simpleDeclare) shouldBe Right(simpleAST)
  }

  it should "successfully parse multiple simple variable declarations" in {
    WorkflowCompiler(multiSimpleDeclare) shouldBe Right(multiSimpleAST)
  }

  it should "successfully parse multiple constructor declarations" in {
    WorkflowCompiler(multiConstructorDeclare) shouldBe Right(multiConstructorAST)
  }

  it should "successfully parse a multi-type constructor declaration" in {
    WorkflowCompiler(multiTypeDeclare) shouldBe Right(multiTypeAST)
  }

  it should "successfully parse a nested constructor declaration" in {
    WorkflowCompiler(nestedDeclare) shouldBe Right(nestedAST)
  }

  it should "successfully parse a doubly nested constructor declaration" in {
    WorkflowCompiler(doublyNestedDeclare) shouldBe Right(doublyNestedAST)
  }

  it should "return an error with an invalid workflow" in {
    WorkflowCompiler(invalidCode) shouldBe Left(errorMsg)
  }

  it should "create a simple runtime environment from variable declarations" in {
    WorkflowRuntime(simpleRuntime) shouldBe Right(simpleRuntimeMap)
  }

}