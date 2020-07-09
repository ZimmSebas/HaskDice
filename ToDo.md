# To Do

- ~Command structures~
- ~Remodel definitions~
- ~Make a functional Eval~
- ~Decide the return value of eval Command~
- ~Add a monad that keeps the state of variables~
- ~Extend Eval~
- ~Add basic math operators, along with sign and mod (with corresponding errors control)~
- ~Add some command evaluator and test if the return system works.~ (Works)
- ~Change AST to emulate a Generalized algebraic datatype (GADT)~ (GADT branch)
- ~Change Eval to emulate the GADT AST~
- ~Add BoolExp and EvalBoolExp~
- ~Set the monad aside in RandomState.hs~
- Add a TypeChecking System (And use it!)
- Change the IfThenElse and Filter to use Booleans
- Add Let and Expr commands
- Add Repeat and Accumulate commands
- Update error system and update eval.
- Cry because i have to make a Lexer
- Lexer
- Cry again, because i have to make a Parser
- Parser
- Language in general
- Change the language of AST.hs comments to english


## To add, maybe
- Make a double-typed variable system (Either?). Add error (type-error variable).
- Research if double-typed variable is a issue in parser. 

## Questions
- ~Lists vs Multisets~ (Lists wins!)
- ~Should i have a Boolean typed variable system?~ (Nope)
- ~Does commands return something?~ (Yep)
- ~1 dice roll, is a Collection or a Number?~ (Collection)
- ~Should i have a Value variable type, or just Collections?~ (I should)
- Should i have boolean variables?
- Should i use a "Skip" command?
- How does "Print" command works?
- Check if assign is needed (as a eval issue, not only as a command. Probably, yes). 
