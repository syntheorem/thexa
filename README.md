Thexa - Template Haskell lEXical Analyzer
=========================================

Thexa is a Haskell library for generating a *lexical analyzer* (typically shortened to *lexer*) using Template Haskell. Lexical analysis is the process of transforming an input string into a list of tokens, where each token is defined by a *regular expression* (typically shortened to *regex*) that is used to match the input. Lexing is frequently used as the first stage in parsing some sort of structured input, particularly for programming languages.

Rather than lexing input by trying each regex one at a time, all of the regexes used by a lexer can be combined into one large state machine that requires only a single pass over the input. The purpose of a lexer generator is to take this list of regexes and preprocess them into a state machine at compile time. Often this is achieved by using a separate tool such as [Alex](https://www.haskell.org/alex/) that takes a source file with special syntax as input and outputs a normal Haskell file that is then compiled by GHC. In contrast, Thexa uses Template Haskell to allow the lexer rules to be defined with normal Haskell code and generates the lexer at compile-time using a *splice*.

Thexa is entirely Unicode-aware, and operates on UTF-8 encoded input. At the moment, other encodings are not supported, although this is a possible future addition. This does mean that the `text` package is not natively supported as input since it uses UTF-16, so to use `Text` as input you must manually encode it to UTF-8 as a `ByteString`. This may change in the future as `text` is planning to switch to UTF-8.

Usage
-----

A fully explained example of using Thexa can be found in the [`examples`](https://github.com/syntheorem/thexa/tree/master/examples) directory, and more detailed information can be found in the API documentation, but here is a quick summary of what defining a lexer looks like.

First, the language extensions `TemplateHaskell` and `QuasiQuotes` must be enabled. Then the list of lexer rules can be defined in one module:

```Haskell
import Thexa

lexerRules =
  [ [re| [\r\n\t\ ]+ |]
      & skipMatch

  , [re| [a-z A-Z]+ |]
      `onMatch` [|| {- Haskell code to run when this rule matches -} ||]
  ]
```

The `[re| ... |]` syntax uses a *quasi-quoter* called `re` that is exported by Thexa. The quasi-quoter parses a regex at compile time into the `Regex` type. The exact syntax of the regexes parsed by this quasi-quoter is documented in [`SYNTAX.md`](https://github.com/syntheorem/thexa/blob/master/SYNTAX.md).

The `[|| ... ||]` syntax is a *typed expression quotation*, a feature of Template Haskell that takes normal Haskell code, type-checks it in the current context, and converts it to an abstract representation of Haskell code that can later be spliced in somewhere else.

The actual lexer is generated in a separate module like so:

```Haskell
import Thexa

lexer = $$(makeLexer lexerRules)
```

The `$$( ... )` syntax is a *typed expression splice*, which runs the `makeLexer` function at compile time and splices the resulting Haskell code into the current expression. The splice must occur in a different module due to a restriction of Template Haskell: `lexerRules` must be fully compiled by GHC before it can be used in a splice.

Requirements
------------

Thexa requires at least GHC 8.10 due to a particular feature of the `template-haskell` library that was introduced in that version allowing an array of bytes to be directly embedded in the program binary. Without this feature, `makeLexer` would generate very large lists of integer literals that GHC takes a very long time to compile.

Comparison to Alex
--------------------

Generally, Thexa has feature parity with Alex, including lexer modes and arbitrary rule conditions. It was primarily created to see how using Template Haskell could improve the UX over using a separate tool. Some of the advantages are:

* Tooling: because the lexer is defined with normal Haskell code, existing Haskell syntax highlighters should work on it, as well as the [Haskell Language Server](https://github.com/haskell/haskell-language-server), making it more pleasant to write the code defining the lexer.

* Documentation: rather than having to learn the specific Alex syntax and know which functions and types it will generate in your module, all the types and functions used by Thexa are exported and documented by Haddock as normal.

* Abstraction: because the list of rules is a normal Haskell type, it is possible to abstract over common patterns in your rules using normal Haskell functions. The provided [example](https://github.com/syntheorem/thexa/tree/master/examples) demonstrates how this might be done.

* Type Errors: because we use typed expression quotes and splices, types are checked at the location of the quote or splice rather than after the code has been expanded, leading to better error messages when type checking.

However, there are some disadvantages inherent to using Template Haskell:

* Although the Haskell Language Server does generally work with Template Haskell, I have found it to be a bit flaky, intermittently choking when trying to compile splices. HLS is constantly being improved though, so I have faith that this will eventually not be an issue.

* Cross-compilation is not always supported when using Template Haskell, and when it is, it tends to be slow because it requires splices to be evaluated on the target platform.

* Compilation speed can be a bit of an issue, but not a major one. Compiling the example lexer (not including the Thexa library itself) takes about 8 seconds on my machine (Intel Core i7-6700K CPU), and most of that is spent compiling the lexer rules just because the regex quasi-quoter generates a relatively large amount of code to represent the AST of the regex. The `makeLexer` splice is actually compiled fairly quickly.

Code Organization
-----------------

Most of the implementation is in submodules of `Thexa.Internal`, all of which are not considered part of the public API.

- `Thexa.Internal.IntLike`: These modules provide an abstraction over `IntSet` and `IntMap` allowing them to be used with integral types other than `Int`. In my benchmarks, using these data structures when compiling regexes to a DFA is about 2x faster compared to using the normal `Set` and `Map` structures. That said, it's frankly still a premature optimization, because actually generating the DFA turned out to not be a significant cost in terms of compile times.

- `Thexa.Internal.Regex`: These modules implement the regex parser and quasi-quoters, as well as compiling a regex to an NFA.

- `Thexa.Internal.NFA`: Provides the Non-deterministic Finite Automaton implementation that is used as an intermediate format when compiling the regexes.

- `Thexa.Internal.DFA`: Provides the Deterministic Finite Automaton representation which is used in the final representation of the lexer. Also implements the transformation of an NFA into a DFA.

- `Thexa.Internal.Unicode`: These modules handle parsing the Unicode database files at compile time in order to map Unicode properties to their respective character sets.

The remainder of the modules provide the public API.

- `Thexa`: The primary high-level entry-point which re-exports everything that you'd need for a typical lexer.

- `Thexa.Core`: A slightly lower-level entry-point for running the lexer which is less "batteries-included" but more flexible.

- `Thexa.Rules`: Provides the interface for defining the rules which will be used to generate the lexer.

- `Thexa.Position`: Implements a Unicode-aware way to keep track of the current position (i.e., line and column numbers) in the source code being lexically analyzed.

- `Thexa.Regex` and `Thexa.CharSet`: Provides a public API for constructing regex ASTs and character sets without using the quasi-quoters, in case such a thing is needed.
