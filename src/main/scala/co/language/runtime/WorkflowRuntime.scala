package co.language.runtime

import scala.util.parsing.input.Positional
import co.language.lexer.WorkflowLexer
import co.language.compiler.{Location, WorkflowError, WorkflowRuntimeError, WorkflowCompiler}
import co.language.parser._
import scala.collection.mutable.{Map}
import util.control.Breaks._

object WorkflowRuntime {

	val registerErr: String = "no legal value in register found"
	val typeCheckerErr: String = "no attribute typechecker found"
	val illegalAttributeErr: String = "no attribute"
	val attributeTypeErr: String = "illegal type found"
	val incorrectArgcErr: String = "incorrect number of arguments found"
	val variableErr: String = "no variable found"
	def illegalAttributeLabel(name: String): String = return " '" + name + "' found"
	def variableLabel(name: String): String = return " with name '" + name + "'"
	def constructorLabel(name: String): String = return " for constructor '" + name + "'"
	def attributeLabel(name: String): String = return " for attribute '" + name + "'"
	def indexLabel(index: String): String = return " at index " + index
	def registerLabel(name: String): String = return ", expected '" + name + "'"

	def lookupEnv(root: Positional, env: Map[String, Value], register: String): Either[WorkflowRuntimeError, Value] = {
		if(!env.contains(register)) return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), variableErr + variableLabel(register))) 
		return Right(env(register))
	}

	def interpret(root: Positional, env: Map[String, Value], register: Option[String], attrTypeChecker: Option[collection.immutable.Map[String, collection.immutable.Map[String, Any]]]): Either[WorkflowRuntimeError, Map[String, Value]] = {
		root match {
			case AndThen(step1,step2) => {
				var newEnv = Map[String, Value]()
				interpret(step1, env, None, None) match {
					case Left(err) => return Left(err)
					case Right(env) => newEnv = env
				}
				return interpret(step2, newEnv, None, None)
			}
			case Variable(key, value) => {
				return interpret(value, env, Some(key), None)
			}

			case StringValue(value) => { 
				register match {
					case Some(keyToUse) => env(keyToUse) = root.asInstanceOf[Value]
					case None => return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), registerErr + registerLabel("variable name")))
				}
				return Right(env)
			}

			case EnumValue(value) => { 
				register match {
					case Some(keyToUse) => env(keyToUse) = root.asInstanceOf[Value]
					case None => return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), registerErr + registerLabel("variable name")))
				}
				return Right(env)
			}

			case VariableValue(value) => { 
				register match {
					case Some(keyToUse) => {
						lookupEnv(root, env, value) match {
							case Left(err) => return Left(err)
							case Right(value) => env(keyToUse) = root.asInstanceOf[Value]
						}
					}
					case None => return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), registerErr + registerLabel("variable name")))
				}
				return Right(env)
				
			}

			case ConstructorValue(value) => { 
				register match {
					case Some(keyToUse) => {
						env(keyToUse) = root.asInstanceOf[Value]
						checkConstructor(root, value, env, keyToUse) match {
							case Some(err) => return Left(err)
							case None => {}
						}
					}
					case None => return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), registerErr + registerLabel("variable name")))
				}	
				return Right(env)

			}

			case AttributeToList(key, values) => { 
				register match {
					case Some(constructorName) => {
						attrTypeChecker match {
							case Some(typeChecker) => {	
								if(typeChecker.contains(key)) {
									val typeInfo: collection.immutable.Map[String, Any] = typeChecker(key)
									val typeName = typeInfo("class")
									var index = 0
									for(value <- values) {
										checkAttribute(root, key, value, env, typeName, Some(index.toString)) match {
											case Some(err) => return Left(err)
											case None => index += 1
										}
									}
									val typeMin: Int = typeInfo("min").asInstanceOf[Int]
									val typeMax: Int = typeInfo("max").asInstanceOf[Int]
									if(typeMin > index || typeMax < index) return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), incorrectArgcErr + attributeLabel(key)))
									return Right(env)
								} else return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), illegalAttributeErr + illegalAttributeLabel(key) + constructorLabel(constructorName)))
							}
							case None => return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), typeCheckerErr))
						}
					}
					case None => return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), registerErr + registerLabel("constructor name")))
				} 
				return Right(env)
			}

			case AttributeToValue(key, value) => {
				register match {
					case Some(constructorName) => {
						attrTypeChecker match {
							case Some(typeChecker) => {	
								if(typeChecker.contains(key)) {
									val typeInfo: collection.immutable.Map[String, Any] = typeChecker(key)
									val typeName = typeInfo("class")
									val typeMin: Int = typeInfo("min").asInstanceOf[Int]
									val typeMax: Int = typeInfo("max").asInstanceOf[Int]
									if(typeMin != 1 || typeMax != 1) return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), incorrectArgcErr + attributeLabel(key)))
									checkAttribute(root, key, value, env, typeName, None) match {
										case Some(err) => return Left(err)
										case None => return Right(env)
									}
									return Right(env)
								} else return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), illegalAttributeErr + illegalAttributeLabel(key) + constructorLabel(constructorName)))
							}
							case None => return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), typeCheckerErr))
						}
					}
					case None => return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), registerErr + registerLabel("constructor name")))
				}
				return Right(env)
			}
		}
	}

	def loopAttributes(root: Positional, attrs: Seq[Attribute], constructor: Constructor, env: Map[String, Value]): Option[WorkflowRuntimeError] = {
		val mm: collection.immutable.Map[String, collection.immutable.Map[String, Any]] = WorkflowAttributeTypeChecker.typeChecker(constructor.getClass.getSimpleName)
		var reqAttrKeys: Set[String] = Set()
		for(attr <- mm.keySet) if(mm(attr)("min").asInstanceOf[Int] > 0) reqAttrKeys = reqAttrKeys + (attr)
		var foundKeys: Set[String] = Set()

		for(attr <- attrs) {
			attr match {
				case AttributeToValue(key, value) => foundKeys = foundKeys + (key)
				case AttributeToList(key, values) => foundKeys = foundKeys + (key)
			}
			interpret(attr, env, Some(constructor.getClass.getSimpleName), Some(mm)) match {
				case Left(err) => return Some(err)
				case _ => {}
			}
		}
		if(reqAttrKeys != foundKeys) return Some(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), incorrectArgcErr + constructorLabel(constructor.getClass.getSimpleName)))
		return None
	}

	def checkConstructor(root: Positional, constructor: Constructor, env: Map[String, Value], keyToUse: String): Option[WorkflowRuntimeError] = {
		constructor match {
			case App(attrs) => return loopAttributes(root, attrs, constructor, env)
			case Page(attrs) => return loopAttributes(root, attrs, constructor, env)
			case Template(attrs) => return loopAttributes(root, attrs, constructor, env)
			case Component(attrs) => return loopAttributes(root, attrs, constructor, env)
			case Event(attrs) => return loopAttributes(root, attrs, constructor, env)
			case Listener(attrs) => return loopAttributes(root, attrs, constructor, env)
			case Filter(attrs) => return loopAttributes(root, attrs, constructor, env)
			case Connective(attrs) => return loopAttributes(root, attrs, constructor, env)
			case Expression(attrs) => return loopAttributes(root, attrs, constructor, env)
			case Arg(attrs) => return loopAttributes(root, attrs, constructor, env)
			case ReferenceArg(attrs) => return loopAttributes(root, attrs, constructor, env)
			case PrimitiveArg(attrs) => return loopAttributes(root, attrs, constructor, env)
			case Property(attrs) => return loopAttributes(root, attrs, constructor, env)
			case Entity(attrs) => return loopAttributes(root, attrs, constructor, env)
		}
		return None
	}

	def checkAttribute(root: Positional, key: String, value: Value, env: Map[String, Value], typeName: Any, index: Option[String]): Option[WorkflowRuntimeError] = {		
		var resolved: Value = value
		breakable {
			while(true) {
		        resolved match {
					case VariableValue(varName) => {
						if(env.contains(varName)) {
							resolved = env(varName)
						} else return Some(constructAttributeError(root, key, variableErr + variableLabel(varName), index))
					}
					case ConstructorValue(constructed) => {
						constructed.getClass match {
							case `typeName` => return None
							case _ => return Some(constructAttributeError(root, key, attributeTypeErr, index))
						} 
					}
					case _ => break
				}
			}
		}

		resolved.getClass match {
			case `typeName` => return None
			case _ => return Some(constructAttributeError(root, key, attributeTypeErr, index))
		}
	}

	def constructAttributeError(root: Positional, attrName: String, baseErrString: String, index: Option[String]): WorkflowRuntimeError = {
		index match {
			case Some(pos) => return WorkflowRuntimeError(Location(root.pos.line, root.pos.column), baseErrString + attributeLabel(attrName) + indexLabel(pos))
			case None => return WorkflowRuntimeError(Location(root.pos.line, root.pos.column), baseErrString + attributeLabel(attrName))
		}
	}

	// def apply(code: String): Either[WorkflowError, Map[String, Value]] = {
	// 	WorkflowCompiler(code) match { 
	// 		case Left(msg: WorkflowError) => {
	// 			return Left(msg)
	// 		}
	// 		case Right(root: WorkflowAST) => {
	// 			// CORE FUNCTIONALITY (SURROUNDING CODE IS REALLY COMPILER STUFF)
	// 			interpret(root, Map[String, Value](), None, None) match {
	// 				case Left(err) => return Left(err) // THROW RUNTIME ERROR
	// 				case Right(env) => return Right(env)
	// 			}

	// 			// THE RUNTIME DOES SEMANTIC CHECKING AND CODE GENERATION
	// 		}
	// 	}
	// }

	def apply(root: WorkflowAST): Either[WorkflowError, Map[String, Value]] = {
		interpret(root, Map[String, Value](), None, None) match {
			case Left(err) => return Left(err) // THROW RUNTIME ERROR
			case Right(env) => return Right(env)
		}
	}

	/*
       	 CODE GEN ROUGH STEPS (version A - COMPILED)
       	 Assume AST format is simple list of variable declarations
       	 Requires 'app' declaration (entrypoint for "code generation")

       	 2 passes required: 
       	 1. traverse and typecheck all constructors in variables while 
       	 caching metadata by variable key, throw type errors if necessary
       	 	=> O(N)
       	 2. begin code generation at 'app' entrypoint, using cache to 
       	 access metadata while emitting code
       	 	=> <O(N) 
       	 
       	 We argue this implementation of a runtime is optimal for this
       	 particular language for a few reasons:
       	 1. user-friendly 'forced' global scoping: variables can be 
       	 referenced anywhere
       	 2. a parse tree evaluation presents no clear advantages at this 
       	 stage for this kind of app metadata language

       	 CODE GEN ROUGH STEPS (version B - INTERPRETED)
       	 Assume AST format is more traditional tree structure that
       	 is suitable for traversal-based code generation. Treat as 'interpreted'
       	 language where every statement (currently) is a variable declaration 
       	 that can be either a primitive or a keyword instance (which may or 
       	 may not be further composed). 
		 
		 1. traverse and typecheck all constructors in variables while 
       	 caching metadata by variable key, throw type errors if necessary
       	 2. simultaneously emit code for constructors, which probably requires
       	 maintenance of a separate code emission metadata structure mapping
       	 variables to prior code snippets (in the case of composition, for 
       	 example, in which the composed variable's previously emitted code might 
       	 need to be modified) 
       	 	=> O(N)

       	*/
}