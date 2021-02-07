# Splint

[![CI](https://github.com/tfausak/splint/workflows/CI/badge.svg)](https://github.com/tfausak/splint/actions/new)
[![Hackage](https://img.shields.io/hackage/v/splint)](https://hackage.haskell.org/package/splint)
[![Stackage](https://www.stackage.org/package/splint/badge/nightly?label=stackage)](https://www.stackage.org/package/splint)

Splint makes [HLint 3][] available as a [GHC source plugin][]. It is similar to
[hlint-source-plugin][] by Ollie Charles, except that it doesn't have to
re-parse the module in order to lint it.

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

## Trade offs

Running HLint as a GHC source plugin has some upsides:

-   Modules are only linted when they're compiled, so if they haven't changed
    they won't be linted again.

-   HLint's suggestions are reported just like GHC's warnings. They're
    formatted the same way and they're reported at the same time.

-   Each module is only parsed once.

-   Parsing is done by GHC instead of something like `haskell-src-exts`. HLint
    already works like this, but by using a plugin you can be sure that all of
    the versions and options line up correctly.

However it's also got some downsides:

-   Using Splint means adding it as a dependency to the targets you want to
    lint. Normally HLint is either a test dependency or just installed on the
    system.

    You may be able to lessen the impact of this by providing a flag to control
    linting. That way you can enable it locally and in CI, but not require
    everything downstream of you to depend on HLint.

    ``` cabal
    flag lint
      default: False
      manual: True
    library
      if flag(lint)
        build-depends: splint
        ghc-options: -fplugin=Splint
    ```

-   It's slower. I've found that it adds about a tenth of a second per module.

-   You can't use the automated refactorings that HLint provides.

-   Using plugins marks every module as unsafe.

[HLint 3]: https://neilmitchell.blogspot.com/2020/05/hlint-30.html
[GHC source plugin]: https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/extending_ghc.html#source-plugins
[hlint-source-plugin]: https://github.com/ocharles/hlint-source-plugin
