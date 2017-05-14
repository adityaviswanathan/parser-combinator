// TODO: enumerate constructors and their respective maps of attribute names to types 
// to then import in runtime when interpreting code

// val toMatch = Map[String, Any](
// 	"Name" 		-> classOf[StringValue],
// 	"Entity"	-> classOf[EnumValue]
// ) 

package co.parsercombinators.runtime

import co.parsercombinators.parser._

object WorkflowAttributeTypeChecker {
	val typeChecker = Map[String, Map[String, Any]](
		"Property" 	-> Map[String, Any](
			"AppType"		-> classOf[EnumValue],
			"Entity" 		-> classOf[Entity],
			"Name"			-> classOf[StringValue]
		),
		"Entity" 	-> Map[String, Any](
			"Properties"	-> classOf[Property],
			"Name"			-> classOf[StringValue]
		)
	)	
}