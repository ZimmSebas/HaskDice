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
- Try TypeSystem (Work in progress) 
- Add a 'Pretty Print' system
- Cry because i have to make a Lexer
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


## TypeSystem idea (branch typetest) 

- Change the Error system, adding different types of errors (Changing the monad from Maybe to a new Error type) (Working on it)
- Add a TypeChecking System (Working on it)
- Update eval


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
- Check if assign is needed (as a eval issue, not only as a command. Probably, yes). 
- Research if double-typed variable is a issue in parser.
- Change the IfThenElse and Filter to use Booleans
- Should i add functions?
