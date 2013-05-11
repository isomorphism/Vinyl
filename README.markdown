Vinyl is a general solution to the records problem in Haskell using type level strings and other modern GHC features, featuring static structural typing (with a subtyping relation), and automatic row-polymorphic lenses. All this is possible without Template Haskell.

First, install Vinyl from [Hackage](http://hackage.haskell.org/package/vinyl):

    cabal update
    cabal install vinyl

To learn more, [try this tutorial](http://www.jonmsterling.com/posts/2013-04-06-vinyl-modern-records-for-haskell.html).

---

### Experimental hacking

Adding support for sum types in a similar style. Example of use:

    _salamander = Case :: "salamander" |:: String
    _frog = Case :: "frog" |:: String
    _moonCheese = Case :: "moon cheese" |:: Integer

    amphibians = switch $ Of
        :| _salamander :~ putStrLn . (++ " . . .")
        :| _frog :~ (\x -> putStrLn x >> putStrLn " *ribbit* ")

    green x = select x $ Of
        :| _frog :~ const "ribbit"
        :| _moonCheese :~ const "a legit fact"

TODO: More functions, instances, some way to have default cases, ...?


