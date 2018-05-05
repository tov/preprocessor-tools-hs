# preprocessor-tools: Quick-and-dirty Haskell preprocessor

This library provides a quick-and-dirty (but often effective) method for
extending Haskell's syntax using a custom preprocessor.  It parses
Haskell into a bare-bones AST with just enough knowledge of the syntax
to preserve nesting, and then allows transformations on the AST.

See the package
[ixdopp](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/ixdopp)
for an example of how to do this.
