# Haskquill

Haskquill is a library that allows you to create valid SQL queries at compile time using Haskell's metaprogramming capabilities.

Queries are created using QDSL and passed to functions that check for correctness and return compile-time errors if there is a problem. The generated query will also be shown as a hint and can be viewed in the IDE.

Because the query is known at compile time, there is no runtime overhead.

## Examples

You can look in the directory "example". Build this using command: `stack build --flag haskquill:build-example`. 

## Acknowledgement

This library is inspired by the [quill](https://github.com/zio/zio-quill) library written in scala.
