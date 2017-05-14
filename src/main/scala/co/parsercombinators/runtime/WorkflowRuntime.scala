package co.parsercombinators.runtime

import scala.util.parsing.input.Positional
import co.parsercombinators.lexer.WorkflowLexer
import co.parsercombinators.compiler.{Location, WorkflowError, WorkflowRuntimeError, WorkflowCompiler}
import co.parsercombinators.parser._

import scala.collection.mutable.{Map}
import util.control.Breaks._

object WorkflowRuntime {

	val registerErr: String = "no legal name in register found"
	val attributeErr: String = "no such attribute exists for specified constructor with name "
	val attributeTypeErr: String = "illegal type found for attribute "
	val variableErr: String = "no variable found with name "

	def lookupEnv(root: Positional, env: Map[String, Value], register: Option[String]): Either[WorkflowRuntimeError, Value] = {
		register match {
			case Some(keyToUse) => {
				if(!env.contains(keyToUse)) return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), "could not find variable " + keyToUse)) 
				return Right(env(keyToUse))
			}
			case None => return Left(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), "no legal lvalue supplied")) 
		}
	}

	def interpret(root: Positional, env: Map[String, Value], register: Option[String], attrTypeChecker: Option[Map[String, Any]]): Either[WorkflowRuntimeError, Map[String, Value]] = {
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
									
									val mm = collection.mutable.Map[String, Any]() ++= WorkflowAttributeTypeChecker.typeChecker(value.getClass.getSimpleName)
									interpret(attr, env, Some(keyToUse), Some(mm)) match {
										case Left(err) => return Left(err)
										case _ => {}
									}
								}
							}
							case Entity(attrs) => {
								for(attr <- attrs) {
									
									val mm = collection.mutable.Map[String, Any]() ++= WorkflowAttributeTypeChecker.typeChecker(value.getClass.getSimpleName)
									interpret(attr, env, Some(keyToUse), Some(mm)) match {
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

			case AttributeToList(key, values) => { 
				var index = 0

				for(value <- values) {
					checkAttribute(key, value, root, env, register, attrTypeChecker) match {
						case Some(err) => return Left(err)
						case None => index += 1
					}
				}

				return Right(env)
				
			}

			// TODO:
			// 2. check that all attributes are satisfied (not just that each presented one is correctly typed)
			case AttributeToValue(key, value) => {
				checkAttribute(key, value, root, env, register, attrTypeChecker) match {
					case Some(err) => return Left(err)
					case None => return Right(env)
				}
				return Right(env)

			}
		}
	}

	def checkAttribute(key: String, value: Value, root: Positional, env: Map[String, Value], register: Option[String], attrTypeChecker: Option[Map[String, Any]]): Option[WorkflowRuntimeError] = {
		attrTypeChecker match {
			case Some(typeChecker) => {			
				register match {
					case Some(keyToUse) => {
						if(typeChecker.contains(key)) {
							val typeName = typeChecker(key)
							var resolved: Value = value

							breakable {
								while(true) {
							        resolved match {
										case VariableValue(varName) => {
											if(env.contains(varName)) {
												resolved = env(varName)
											} else return Some(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), variableErr + varName))
										}
										case ConstructorValue(constructed) => {
											constructed.getClass match {
												case `typeName` => return None
												case _ => return Some(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), attributeTypeErr + key))
											} 
										}
										case _ => break
									}
								}
							}

							resolved.getClass match {
								case `typeName` => return None
								case _ => return Some(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), attributeTypeErr + key))
							}
						} else return Some(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), attributeErr + key))
					}
					case None => return Some(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), registerErr))
				}
				return None
			}
			case None => return Some(WorkflowRuntimeError(Location(root.pos.line, root.pos.column), registerErr))
		}
	}

	def apply(code: String): Either[WorkflowError, Map[String, Value]] = {
		WorkflowCompiler(code) match { 
			case Left(msg: WorkflowError) => {
				return Left(msg)
			}
			case Right(root: WorkflowAST) => {
				// CORE FUNCTIONALITY (SURROUNDING CODE IS REALLY COMPILER STUFF)
				interpret(root, Map[String, Value](), None, None) match {
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