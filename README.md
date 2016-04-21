# haskell-interop-prime

  This library is based heavily off of https://github.com/pkamenarsky/purescript-interop. A great many
  thanks to Philip Kamenarsky. I was going to contribute to his library, but there are just way too many
  changes at this point. The basic idea is this: I want to write my data types in Haskell, then generate
  JSON instances for Haskell, equivalent types & JSON instances for other languages, and potentially even
  API hooks. Without a library like this, I find myself spending way too much time duplicating my efforts
  by writing the same types & JSON instances for both haskell and purescript.

  This library is extremely experimental.


## Examples

Example:
- https://github.com/adarqui/haskell-interop-prime/blob/master/src/Haskell/Interop/Prime/Test.hs

Examples of generated code:
- https://github.com/adarqui/haskell-interop-prime/tree/master/gen
