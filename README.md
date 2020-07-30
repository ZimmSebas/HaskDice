# HaskDice

A Deep EDSL for specification of dice rolls, making possible to program a diceroll query in a easy and practical way.

Made in Haskell. Inspired by Troll language. 

## Prerequisites

- Haskell
- [Stack](https://docs.haskellstack.org/en/stable/README/)

## Build

You can install HaskDice with stack, using


```
stack setup
stack build
stack install
```

That should install the libraries and dependencies, and then copy the executable to the local bin.

## Run

Once installed, you can use HaskDice tu run programs from console, for example:

```
haskdice test.hkd
```

Or you can call the interactive mode, using the flag -i

```
haskdice -i
```

This allow you to parse and eval expressions in an interactive mode, and you can load files using the ":load" command.

For more information about the uses of the language check the "InformeFinal-HaskDice" that explains uses of the language (is in Spanish, sorry D: )

### License

GNU General Public License v3.0

### Contributors

Made by @ZimmSebas, with love <3


This EDSL, was made as my final project for the Programming language analysis subject.
