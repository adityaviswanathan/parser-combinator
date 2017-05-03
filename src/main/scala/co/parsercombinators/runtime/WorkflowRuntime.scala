package co.parsercombinators.runtime

import scala.util.parsing.input.Positional
import co.parsercombinators.compiler.{WorkflowCompilationError}
import co.parsercombinators.lexer.WorkflowLexer
import co.parsercombinators.compiler.WorkflowCompiler
import co.parsercombinators.parser._

import scala.reflect.runtime.universe._
import scala.collection.mutable.{Map}

object WorkflowRuntime {

	def traverseAST(root: Positional, env: Map[String, Value], envKey: Option[String]): Map[String, Value] = {
		root match {

			case Variable(key, value) => {
				return traverseAST(value, env, Some(key))
			}

			case StringValue(value) => { 
				envKey match {
					case Some(keyToUse) => env(keyToUse) = root.asInstanceOf[Value]
					case None => println("ERROR IN STRING!")
				}
				return env
			}
			case EnumValue(value) => { 
				envKey match {
					case Some(keyToUse) => env(keyToUse) = root.asInstanceOf[Value]
					case None => println("ERROR IN STRING!")
				}
				return env
			}
			case ConstructorValue(value) => { 
				value match {
					case Property(attrs) => {
						println("IN PROPS BLOCK")
						for(attr <- attrs) {
							println(attr)
						}
					}
				}

				envKey match {
					case Some(keyToUse) => env(keyToUse) = root.asInstanceOf[Value]
					case None => println("ERROR IN CONSTRUCTOR!")
				}
				return env

			}
			case VariableValue(value) => { 
				envKey match {
					case Some(keyToUse) => env(keyToUse) = root.asInstanceOf[Value]
					case None => println("ERROR IN VARIABLE!")
				}
				return env
			}

			case AndThen(step1,step2) => {
				val newEnv: Map[String, Value] = traverseAST(step1, env, None)
				return traverseAST(step2, newEnv, None)
			}

		}
	}

	// def apply(code: String) = {
	def apply() = {
		// val ast = WorkflowCompiler(code)
		
		val multiSimpleDeclare = 
		    """
		      |dummy = "newString"
		      |dummy2 = dummy
		      |dummy3 = ENUM.COMPONENT.STATIC
		      |dummy4 = Property( 
		      |  Name : "PropName",
		      |  AnotherProp: [dummy, dummy2] 
		      |)
		    """.stripMargin.trim
		WorkflowCompiler(multiSimpleDeclare) match { 
			case Left(msg: WorkflowCompilationError) => {
				println(msg)
			}
			case Right(root: WorkflowAST) => {
				println(traverseAST(root, Map[String, Value](), None))
			}
		}

		object traverser extends Traverser {
        	var applies = List[Apply]()
        	override def traverse(tree: Tree): Unit = tree match {
          		case app @ Apply(fun, args) =>
	            	applies = app :: applies
	            	super.traverse(fun)
	            	super.traverseTrees(args)
          		case _ => super.traverse(tree)
        	}
       	}

       	// traverser.traverse(ast)
       	// println(traverser.applies)

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
}