package co.parsercombinators.runtime

import scala.util.parsing.input.Positional
import co.parsercombinators.lexer.WorkflowLexer
import co.parsercombinators.compiler.{Location, WorkflowError, WorkflowRuntimeError, WorkflowCompiler}
import co.parsercombinators.parser._

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.collection.mutable.{Map}

object WorkflowRuntime {

	val registerErr: String = "no legal name in register found"
	val constructorErr: String = "illegal type found for attribute "

	def lookupEnv(root: Positional, env: Map[String, Value], register: Option[String]): Either[WorkflowRuntimeError, Value] = {
		register match {
			case Some(keyToUse) => {
				if(!env.contains(keyToUse)) return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), "could not find variable " + keyToUse)) 
				return Right(env(keyToUse))
			}
			case None => return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), "no legal lvalue supplied")) 
		}
	}

	def interpret(root: Positional, env: Map[String, Value], register: Option[String]): Either[WorkflowRuntimeError, Map[String, Value]] = {
		root match {
			case AndThen(step1,step2) => {
				var newEnv = Map[String, Value]()
				interpret(step1, env, None) match {
					case Left(err) => return Left(err)
					case Right(env) => newEnv = env
				}
				return interpret(step2, newEnv, None)
			}
			case Variable(key, value) => {
				return interpret(value, env, Some(key))
			}

			case StringValue(value) => { 
				register match {
					case Some(keyToUse) => env(keyToUse) = root.asInstanceOf[Value]
					case None => return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), registerErr))
				}
				return Right(env)
			}

			case EnumValue(value) => { 
				register match {
					case Some(keyToUse) => env(keyToUse) = root.asInstanceOf[Value]
					case None => return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), registerErr))
				}
				return Right(env)
			}

			case VariableValue(value) => { 
				lookupEnv(root, env, Some(value)) match {
					case Left(err) => { 
						return Left(err)
					}
					case Right(value) => { 
						register match {
							case Some(keyToUse) => env(keyToUse) = root.asInstanceOf[Value]
							case None => return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), registerErr))
						}
					}
				}
				return Right(env)
			}

			case ConstructorValue(value) => { 
				register match {
					case Some(keyToUse) => {
						env(keyToUse) = root.asInstanceOf[Value]
						value match {
							case Property(attrs) => {
								for(attr <- attrs) {
									interpret(attr, env, Some(keyToUse)) match {
										case Left(err) => return Left(err)
										case _ => {}
									}
								}
							}
						}
					}
					case None => return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), registerErr))
				}	
				return Right(env)

			}

			case AttributeToValue(key, value) => {
				val toMatch = Map[String, Any](
					"Name" 		-> StringValue,
					"Entity"	-> EnumValue
				)

				var found = Set[String]()				

				register match {
					case Some(keyToUse) => {
						found += key
						if(toMatch.contains(key)) {
							val typeName = toMatch(key)

							// THE CODE BELOW IS A TEMPORARY HACK TO GET AROUND 
							// COMPARING THE RUNTIME TYPE OF A VARIABLE TO A 
							// HASHED-TO 'ANY' TYPE. EVENTUALLY THIS WILL NEED 
							// TO BE FIXED BUT WE PROCEED FOR NOW TO GET TO AN 
							// E2E SYSTEM ASAP

							if(workAround(value.toString, typeName.toString)) {
								return Right(env)
							} else {
								return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), constructorErr + key))
							}

							// TODO: fix the below:
							// capture runtime type of variable and compare with expected type
							// currently running into issues getting this transformation done: 
							// StringValue("hello") -> StringValue

							// value match {
							// 	case `typeName` => {
							// 		println("YAY HERE YAY :)")
							// 	}
							// 	case _ => {
									
							// 		println("HIT HERE AGAIN :(")
							// 		return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), constructorErr + key))
							// 	}
							// }
						} else return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), constructorErr + key))
					}
					case None => return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), registerErr))
				}
				return Right(env)
			}
		}
	}

	// THIS DIDN'T WORK
	def getTypeTag[T: ru.TypeTag](obj: T) = ru.typeTag[T]

	def workAround(foundValue: String, valueToMatch: String): Boolean = {
		val valueType = foundValue
		val foundTypeNameWithArgs = valueType.split('(')
		val foundTypeName: String = foundTypeNameWithArgs(0)
		if(foundTypeName == valueToMatch) return true
		return false
	}

	def apply(code: String): Either[WorkflowError, Map[String, Value]] = {
		WorkflowCompiler(code) match { 
			case Left(msg: WorkflowError) => {
				return Left(msg)
			}
			case Right(root: WorkflowAST) => {
				// CORE FUNCTIONALITY (SURROUNDING CODE IS REALLY COMPILER STUFF)
				interpret(root, Map[String, Value](), None) match {
					case Left(err) => return Left(err)
					case Right(env) => return Right(env)
				}
			}
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