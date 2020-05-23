# Splint

Splint is a proof of concept, showing how to use [HLint 3][] as a [GHC source
plugin][]. It is similar to [hlint-source-plugin][] by Ollie Charles, except
that it doesn't have to re-parse the module in order to lint it.

Splint isn't on Hackage yet, so the only way to install it is to clone the Git
repository. And like HLint 3, it only builds with GHC 8.10. (I think both of
these constraints could be relaxed, but Splint is just a toy at this point.)

To use Splint, pass `-fplugin=Splint` to GHC. Any ideas suggested by HLint will
be reported as warnings by GHC. For example, if you define `Main.hs` as:

``` hs
main = print . concat $ map pure [ 'a' .. 'z' ]
```

You would expect HLint to tell you to use `concatMap`. Normally you would need
to both compile your module with GHC and lint it with HLint. However with
Splint you can compile it and get suggestions from HLint all at once by
running:

``` sh
ghc -fplugin=Splint Main.hs
```

Among all the usual output from GHC, you should see this new warning:

```
Main.hs:1:8: warning:
    Use concatMap
    Perhaps: print (concatMap pure ['a' .. 'z'])
  |
1 | main = print . concat $ map pure [ 'a' .. 'z' ]
  |        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

And that's all there is to it! HLint suggestions as part of your normal build
process. What a time to be alive.

If you want to pass arguments through to HLint, you can use
`-fplugin-opt=Splint:arg`. For example you can ignore the warning above with
`-fplugin-opt=Splint:'--ignore=Use concatMap'`.

## To do

As stated, Splint is basically a tech demo. Although it's usable in its current
form, there is some low hanging fruit to fix before it should be considered
ready for production:

- [ ] Work with versions of GHC older than 8.10.
- [x] Accept command-line options using `-fplugin-opt=Splint:something`.
- [x] Reliably read HLint configuration.
- [x] Avoid re-reading HLint config for each source file.
- [x] Figure out a good output format for the warnings.
- [ ] Publish to Hackage.

[HLint 3]: https://neilmitchell.blogspot.com/2020/05/hlint-30.html
[GHC source plugin]: https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/extending_ghc.html#source-plugins
[hlint-source-plugin]: https://github.com/ocharles/hlint-source-plugin
