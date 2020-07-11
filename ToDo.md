# To Do

- ~Command structures~
- ~Remodel definitions~
- ~Make a functional Eval~
- ~Decide the return value of eval Command~
- ~Add a monad that keeps the state of variables~
- ~Extend Eval~
- ~Add basic math operators, along with sign and mod (with corresponding errors control)~
- ~Add some command evaluator and test if the return system works.~ (Works)
- ~Try with GADT~ (Done, works, now in master branch)
- ~Try TypeSystem~
- ~Add a 'Pretty Print' system~
- Modify ACCUM and REPUNT for better uses
- Test Eval with complicated cases (Work in progress)
- Cry because i have to make a Lexer (Work in progress)
- Lexer
- Cry again, because i have to make a Parser
- Parser
- Add the file and interactive mode
- Add cabal package and command system
- Change the language of AST.hs comments to english


## GADT idea (GADT branch) (Done, added to master branch)
- ~Change AST to emulate a Generalized algebraic datatype (GADT)~ (GADT branch)
- ~Change Eval to emulate the GADT AST~
- ~Add BoolExp and EvalBoolExp~
- ~Set the monad aside in RandomState.hs~
- ~Define a "IsEmpty" format for all variables~ (Done!) 
- ~Add Let and Expr commands~
- ~Add Repeat and Accumulate commands~

It works! :D


## TypeSystem idea (typetest branch ) (Done, added to master branch)

- ~Add typing System~ :D
- ~Add typing Checks~ 
- ~Improve typingValue. Add typingVar and typingCommand.~
- ~Add a Enviroment of Type Variables. Maybe i need to change the monad for typing system.~ (New monad indeed)
- ~Add a show instance for Expressions and Commands~
- ~Add a show instance for Error~
- ~Add a show instance for Result~
- ~Change the Error system, adding different types of errors (Changing the monad from Maybe to a new Error type)~
- ~Update eval~

It works! :D

### Questions and Researchs
- ~Lists vs Multisets~ (Lists wins!)
- ~Should i have a Boolean typed variable system?~ (Nope)
- ~Does commands return something?~ (Yep)
- ~1 dice roll, is a Collection or a Number?~ (Collection)
- ~Should i have a Value variable type, or just Collections?~ (I should)
- ~Should i have boolean variables?~ (Yeap)
- ~Should i use a "Skip" command?~ (Nope)
- ~How does "Print" command works?~ (Destroyed print)
- ~Make a double-typed variable system (Either?). Add error (type-error variable).~ (Added)
- ~Check if update should have returns~ (Can, but not should)
- ~Check if assign is needed (as a eval issue, not only as a command. Probably, yes).~ (Seems not)
- Should i have a possible If Then (without else)
- Research if double-typed variable is a issue in parser.
- Change the IfThenElse and Filter to use Booleans
- Should i add functions?
