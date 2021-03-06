package co.language

import co.language.compiler.{Location, WorkflowCompiler, WorkflowParserError, WorkflowRuntimeError}
import co.language.parser._
import org.scalatest.{FlatSpec, Matchers}
import co.language.runtime.WorkflowRuntime
import co.language.generator.WorkflowGenerator

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
      |  Name : dummy2
      |)
      |dummy2 = dummy4
    """.stripMargin.trim

  val complexAttributeRuntime = 
    """
      |dummy = "val"
      |dummy2 = "val2"
      |prop = PROPERTY( 
      |  Name : dummy2
      |)
      |ent = ENTITY(
      |  Name : dummy,
      |  Properties : [PROPERTY( 
      |    Name : dummy
      |  ), prop]
      |)
      |dummy4 = PROPERTY( 
      |  Name : dummy2
      |)
    """.stripMargin.trim

  val mixMatchArgcRuntime = 
    """
      |dummy2 = "val2"
      |prop = PROPERTY( 
      |  Name : [dummy2]
      |)
      |ent = ENTITY(
      |  Name : [dummy2],
      |  Properties : [PROPERTY( 
      |    Name : dummy
      |  ), prop]
      |)
    """.stripMargin.trim

  val illegalSimpleRuntime = 
    """
    |dummy = "firststring"
    |dummy2 = "secondstring"
    |dummy3 = dummy2
    |dummy4 = dummy4
    """.stripMargin.trim

  val illegalAttributeArgc = 
    """
      |dummy = "val"
      |dummy2 = "val2"
      |prop = PROPERTY( 
      |  Name : dummy2
      |)
      |ent = ENTITY(
      |  Name : [dummy, dummy2],
      |  Properties : [prop]
      |)
    """.stripMargin.trim

  val illegalConstructorArgc = 
    """
      |dummy2 = "val2"
      |ent = ENTITY(
      |  Name : [dummy2]
      |)
    """.stripMargin.trim

  val illegalAttribute = 
    """
      |dummy2 = "val2"
      |prop = PROPERTY( 
      |  Name : [dummy2],
      |  IllegalThing : dummy2
      |)
    """.stripMargin.trim

  val illegalAttributeType = 
    """
      |dummy2 = ENUM.COMPONENT.STATIC
      |prop = PROPERTY( 
      |  Name : dummy2
      |)
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
        AttributeToValue("Name",VariableValue("dummy2"))
      ))), 
    "dummy" -> StringValue("tester"), 
    "dummy3" -> EnumValue(Enum("ComponentType","STATIC")), 
    "dummy2" -> VariableValue("dummy4")
  )

  val complexAttributeRuntimeMap = Map(
    "dummy" -> StringValue("val"),
    "dummy2" -> StringValue("val2"),
    "prop" -> ConstructorValue(Property(
      List(
        AttributeToValue("Name",VariableValue("dummy2"))
      ))),
    "ent" -> ConstructorValue(Entity(
      List(
        AttributeToValue("Name",VariableValue("dummy")),
        AttributeToList("Properties",List(
          ConstructorValue(Property(
            List(
              AttributeToValue("Name",VariableValue("dummy"))
            ))),
          VariableValue("prop")
        ))
      ))), 
    "dummy4" -> ConstructorValue(Property(
      List(
        AttributeToValue("Name",VariableValue("dummy2"))
      )))
  )

  val mixMatchArgcRuntimeMap = Map(
    "dummy2" -> StringValue("val2"),
    "prop" -> ConstructorValue(Property(
      List(
        AttributeToList("Name",List(
          VariableValue("dummy2")
        ))
      ))),
    "ent" -> ConstructorValue(Entity(
      List(
        AttributeToList("Name",List(
          VariableValue("dummy2")
        )),
        AttributeToList("Properties",List(
          ConstructorValue(Property(
            List(
              AttributeToValue("Name",VariableValue("dummy"))
            ))),
          VariableValue("prop")
        ))
      )))
  )


  val illegalSimpleRuntimeErr = WorkflowRuntimeError(Location(4,10), "no variable found with name 'dummy4'")
  val illegalAttributeArgcErr = WorkflowRuntimeError(Location(7,3), "incorrect number of arguments found for attribute 'Name'")
  val illegalConstructorArgcErr = WorkflowRuntimeError(Location(2,7), "incorrect number of arguments found for constructor 'Entity'")
  val illegalAttributeErr = WorkflowRuntimeError(Location(4,3), "no attribute 'IllegalThing' found for constructor 'Property'")
  val illegalAttributeTypeErr = WorkflowRuntimeError(Location(3,3), "illegal type found for attribute 'Name'")

  "Workflow compiler" should "successfully parse a simple variable declaration" in {
    WorkflowCompiler.syntax(simpleDeclare) shouldBe Right(simpleAST)
  }

  it should "successfully parse multiple simple variable declarations" in {
    WorkflowCompiler.syntax(multiSimpleDeclare) shouldBe Right(multiSimpleAST)
  }

  it should "successfully parse multiple constructor declarations" in {
    WorkflowCompiler.syntax(multiConstructorDeclare) shouldBe Right(multiConstructorAST)
  }

  it should "successfully parse a multi-type constructor declaration" in {
    WorkflowCompiler.syntax(multiTypeDeclare) shouldBe Right(multiTypeAST)
  }

  it should "successfully parse a nested constructor declaration" in {
    WorkflowCompiler.syntax(nestedDeclare) shouldBe Right(nestedAST)
  }

  it should "successfully parse a doubly nested constructor declaration" in {
    WorkflowCompiler.syntax(doublyNestedDeclare) shouldBe Right(doublyNestedAST)
  }

  it should "return an error with an invalid workflow" in {
    WorkflowCompiler.syntax(invalidCode) shouldBe Left(errorMsg)
  }

  it should "create a simple runtime environment from variable declarations" in {
    WorkflowCompiler(simpleRuntime) shouldBe Right(simpleRuntimeMap)
  }

  it should "create a complex runtime environment with attribute lists and combined inline declarations/variables" in {
    WorkflowCompiler(complexAttributeRuntime) shouldBe Right(complexAttributeRuntimeMap)
  }

  it should "create a runtime environment with varied attribute values/lists as long as argc is legal" in {
    WorkflowCompiler(mixMatchArgcRuntime) shouldBe Right(mixMatchArgcRuntimeMap)
  }

  it should "return an error when an undeclared variable is referenced" in {
    WorkflowCompiler(illegalSimpleRuntime) shouldBe Left(illegalSimpleRuntimeErr)
  }

  it should "return an error when an illegal number of arguments are passed to an attribute" in {
    WorkflowCompiler(illegalAttributeArgc) shouldBe Left(illegalAttributeArgcErr)
  }

  it should "return an error when an illegal number of arguments (attributes) are passed to a constructor" in {
    WorkflowCompiler(illegalConstructorArgc) shouldBe Left(illegalConstructorArgcErr)
  }

  it should "return an error when an illegal attribute is passed to a constructor" in {
    WorkflowCompiler(illegalAttribute) shouldBe Left(illegalAttributeErr)
  }

  it should "return an error when an illegal argument type is passed to an attribute" in {
    WorkflowCompiler(illegalAttributeType) shouldBe Left(illegalAttributeTypeErr)
  }

}

class WorkflowGeneratorSpec extends FlatSpec with Matchers {
  val complexAttributeRuntimeMap = Map(
    "dummy" -> StringValue("val"),
    "dummy2" -> StringValue("val2"),
    "prop" -> ConstructorValue(Property(
      List(
        AttributeToValue("Name",VariableValue("dummy2"))
      ))),
    "ent" -> ConstructorValue(Entity(
      List(
        AttributeToValue("Name",VariableValue("dummy")),
        AttributeToList("Properties",List(
          ConstructorValue(Property(
            List(
              AttributeToValue("Name",VariableValue("dummy"))
            ))),
          VariableValue("prop"),
          ConstructorValue(Property(
            List(
              AttributeToValue("Name",StringValue("baby"))
            )))
        ))
      ))),
    "ent2" -> VariableValue("ent"), 
    "dummy4" -> ConstructorValue(Property(
      List(
        AttributeToValue("Name",VariableValue("dummy2"))
      ))),
    "app" -> ConstructorValue(App(
      List(
        AttributeToList("Entities",List(VariableValue("ent2")))
      )))
  )

  "Workflow generator" should "initialize database when supplied legal entities and properties" in {
    WorkflowGenerator(complexAttributeRuntimeMap)
  }
}