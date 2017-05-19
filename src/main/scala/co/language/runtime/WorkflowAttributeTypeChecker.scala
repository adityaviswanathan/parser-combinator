package co.language.runtime

import co.language.parser._

object WorkflowAttributeTypeChecker {
	val typeChecker = Map[String, Map[String, Map[String, Any]]](
		"Page" 		-> Map[String, Map[String, Any]](
			"Templates"		-> Map[String, Any](
				"class" -> classOf[Template],
				"min"	-> 0,
				"max"	-> 0 ),
			"Name"			-> Map[String, Any](
				"class" -> classOf[StringValue],
				"min"	-> 0,
				"max"	-> 0 )
 		),
		"Template" 		-> Map[String, Map[String, Any]](
			"Components"	-> Map[String, Any](
				"class" -> classOf[Component],
				"min"	-> 0,
				"max"	-> 0 ),
			"Events"		-> Map[String, Any](
				"class" -> classOf[Event],
				"min"	-> 0,
				"max"	-> 0 ),
			"Listeners"		-> Map[String, Any](
				"class" -> classOf[Listener],
				"min"	-> 0,
				"max"	-> 0 )
		),
		"Component" 		-> Map[String, Map[String, Any]](
			"ComponentType"	-> Map[String, Any](
				"class" -> classOf[EnumValue],
				"min"	-> 0,
				"max"	-> 0 ),
			"ElementType"	-> Map[String, Any](
				"class" -> classOf[EnumValue],
				"min"	-> 0,
				"max"	-> 0 ),
			"Row"			-> Map[String, Any](
				"class" -> classOf[StringValue],
				"min"	-> 0,
				"max"	-> 0 ),
			"Column"		-> Map[String, Any](
				"class" -> classOf[StringValue],
				"min"	-> 0,
				"max"	-> 0 ),
			"Width"			-> Map[String, Any](
				"class" -> classOf[StringValue],
				"min"	-> 0,
				"max"	-> 0 ),
			"Offset"		-> Map[String, Any](
				"class" -> classOf[StringValue],
				"min"	-> 0,
				"max"	-> 0 )
		),
		"Event" 		-> Map[String, Map[String, Any]](
			"EventType"		-> Map[String, Any](
				"class" -> classOf[EnumValue],
				"min"	-> 0,
				"max"	-> 0 ),
			"Component"		-> Map[String, Any](
				"class" -> classOf[Component],
				"min"	-> 0,
				"max"	-> 0 ),
			"Page"			-> Map[String, Any](
				"class" -> classOf[Page],
				"min"	-> 0,
				"max"	-> 0 )
		),
		"Listener" 		-> Map[String, Map[String, Any]](
			"ListenerType"	-> Map[String, Any](
				"class" -> classOf[EnumValue],
				"min"	-> 0,
				"max"	-> 0 ),
			"Event"			-> Map[String, Any](
				"class" -> classOf[Event],
				"min"	-> 0,
				"max"	-> 0 ),
			"Filters"		-> Map[String, Any](
				"class" -> classOf[Filter],
				"min"	-> 0,
				"max"	-> 0 )
		),
		"Filter" 		-> Map[String, Map[String, Any]](
			"Expression"	-> Map[String, Any](
				"class" -> classOf[Expression],
				"min"	-> 0,
				"max"	-> 0 ),
			"Listener"		-> Map[String, Any](
				"class" -> classOf[Listener],
				"min"	-> 0,
				"max"	-> 0 )
		),
		"Connective" 	-> Map[String, Map[String, Any]](
			"ConnectiveType"-> Map[String, Any](
				"class" -> classOf[EnumValue],
				"min"	-> 0,
				"max"	-> 0 ),
			"Expressions"	-> Map[String, Any](
				"class" -> classOf[Expression],
				"min"	-> 0,
				"max"	-> 0 )
		),
		"Expression" 	-> Map[String, Map[String, Any]](
			"OperatorType"	-> Map[String, Any](
				"class" -> classOf[EnumValue],
				"min"	-> 0,
				"max"	-> 0 ),
			"Args"			-> Map[String, Any](
				"class" -> classOf[Arg],
				"min"	-> 0,
				"max"	-> 0 )
		),
		"Arg" 			-> Map[String, Map[String, Any]](
			"ArgType"		-> Map[String, Any](
				"class" -> classOf[EnumValue],
				"min"	-> 0,
				"max"	-> 0 ),
			"Expression"	-> Map[String, Any](
				"class" -> classOf[Expression],
				"min"	-> 0,
				"max"	-> 0 ),
			"Component"		-> Map[String, Any](
				"class" -> classOf[Component],
				"min"	-> 0,
				"max"	-> 0 ),
			"Reference"		-> Map[String, Any](
				"class" -> classOf[ReferenceArg],
				"min"	-> 0,
				"max"	-> 0 ),
			"Primitive"		-> Map[String, Any](
				"class" -> classOf[PrimitiveArg],
				"min"	-> 0,
				"max"	-> 0 )
		),
		"ReferenceArg" 	-> Map[String, Map[String, Any]](
			"Entity"		-> Map[String, Any](
				"class" -> classOf[Entity],
				"min"	-> 0,
				"max"	-> 0 ),
			"Property"		-> Map[String, Any](
				"class" -> classOf[Property],
				"min"	-> 0,
				"max"	-> 0 )
		),
		"PrimitiveArg" 	-> Map[String, Map[String, Any]](
			"AppType"		-> Map[String, Any](
				"class" -> classOf[EnumValue],
				"min"	-> 0,
				"max"	-> 0 ),
			"Value"			-> Map[String, Any](
				"class" -> classOf[StringValue],
				"min"	-> 0,
				"max"	-> 0 )
		),
		"Property" 		-> Map[String, Map[String, Any]](
			"AppType"		-> Map[String, Any](
				"class" -> classOf[EnumValue],
				"min"	-> 0,
				"max"	-> 0 ),
			"Name"			-> Map[String, Any](
				"class" -> classOf[StringValue],
				"min"	-> 1,
				"max"	-> 1 )
		),
		"Entity" 		-> Map[String, Map[String, Any]](
			"Properties"	-> Map[String, Any](
				"class" -> classOf[Property],
				"min"	-> 1,
				"max"	-> 100 ),
			"Name"			-> Map[String, Any](
				"class" -> classOf[StringValue],
				"min"	-> 1,
				"max"	-> 1 )
		)
	)	
}