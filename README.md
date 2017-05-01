## Parser combinator playground


Currently `master` houses support for a basic language that supports conditionals and attempts variable declaration. This is organized into an AST (yet to be evaluated). So far this a TDD project.  

On the roadmap:

- Design and implement a runtime
    - Environment variables and context
    - Validate constructors and attributes (potentially can be treated as closures)
    - Evaluation build around root `App` AST node
- Flexible code generation
    - Design language-agnostic functionality stubs linked together by namespaces originating from coded variables
- Add to and flesh out testing suite 

### Run tests

```
> sbt test
```
### CLI-style usage

```
> sbt console
> ...
scala> import co.parsercombinators.compiler.WorkflowCompiler
import co.parsercombinators.compiler.WorkflowCompiler
scala> val multiSimpleDeclare = 
     |     """
     |       |dummy = "newString"
     |       |dummy2 = dummy
     |       |dummy3 = ENUM.COMPONENT.STATIC
     |     """.stripMargin.trim
scala> ...
scala> WorkflowCompiler(multiSimpleDeclare)
res1: Either[co.parsercombinators.compiler.WorkflowCompilationError,co.parsercombinators.parser.WorkflowAST] = Right...

```


