# haskell-interop-prime

  **WIP**

  This library is based heavily off of https://github.com/pkamenarsky/purescript-interop. A great many
  thanks to Philip Kamenarsky. I was going to contribute to his library, but there are just way too many
  changes at this point. The basic idea is this: I want to write my data types in Haskell, then generate
  JSON instances for Haskell, equivalent types & JSON instances for other languages, and potentially even
  API hooks. Without a library like this, I find myself spending way too much time duplicating my efforts
  by writing the same types & JSON instances for both haskell and purescript.

  This library is extremely experimental.

## eventual refactor

I'm still figuring out what I need from this library. Eventually it will need a major refactor. Until then,
i'd like to generate more instances for Haskell: Read, Eq, Ord, Enum, Show etc. I'd like to generate equivalent
code between Haskell & Purescript so that, in my haskell source files, the only thing i'm deriving is Generic & Typeable.

This code is a train wreck. LMAO. I'm starting to enjoy adding more spaghetti. Though, it is useful. So, tasty spaghetti.

## Examples

Example:
- https://github.com/adarqui/haskell-interop-prime/blob/master/src/Haskell/Interop/Prime/Test.hs

Examples of generated code:
- https://github.com/adarqui/haskell-interop-prime/tree/master/gen
