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
		"Page" 		-> Map[String, Any](
			"Templates"		-> classOf[Template],
			"Name"			-> classOf[StringValue]
		),
		"Template" 		-> Map[String, Any](
			"Components"	-> classOf[Component],
			"Events"		-> classOf[Event],
			"Listeners"		-> classOf[Listener]
		),
		"Component" 		-> Map[String, Any](
			"ComponentType"	-> classOf[EnumValue],
			"ElementType"	-> classOf[EnumValue],
			"Row"			-> classOf[StringValue],
			"Column"		-> classOf[StringValue],
			"Width"			-> classOf[StringValue],
			"Offset"		-> classOf[StringValue]
		),
		"Event" 		-> Map[String, Any](
			"EventType"		-> classOf[EnumValue],
			"Component"		-> classOf[Component],
			"Page"			-> classOf[Page]
		),
		"Listener" 		-> Map[String, Any](
			"ListenerType"	-> classOf[EnumValue],
			"Event"			-> classOf[Event],
			"Filters"		-> classOf[Filter]
		),
		"Filter" 		-> Map[String, Any](
			"Expression"	-> classOf[Expression],
			"Listener"		-> classOf[Listener]
		),
		"Connective" 	-> Map[String, Any](
			"ConnectiveType"-> classOf[EnumValue],
			"Expressions"	-> classOf[Expression]
		),
		"Expression" 	-> Map[String, Any](
			"OperatorType"	-> classOf[EnumValue],
			"Args"			-> classOf[Arg]
		),
		"Arg" 			-> Map[String, Any](
			"ArgType"		-> classOf[EnumValue],
			"Expression"	-> classOf[Expression],
			"Component"		-> classOf[Component],
			"Reference"		-> classOf[ReferenceArg],
			"Primitive"		-> classOf[PrimitiveArg]
		),
		"ReferenceArg" 	-> Map[String, Any](
			"Entity"		-> classOf[Entity],
			"Property"		-> classOf[Property]
		),
		"PrimitiveArg" 	-> Map[String, Any](
			"AppType"		-> classOf[EnumValue],
			"Value"			-> classOf[StringValue]
		),
		"Property" 		-> Map[String, Any](
			"AppType"		-> classOf[EnumValue],
			"Name"			-> classOf[StringValue]
		),
		"Entity" 		-> Map[String, Any](
			"Properties"	-> classOf[Property],
			"Name"			-> classOf[StringValue]
		)
	)	
}