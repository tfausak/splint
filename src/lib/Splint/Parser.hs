{-# LANGUAGE CPP #-}

-- | Starting with HLint 3, it's possible to re-use the parsed module that GHC
-- produces to avoid re-parsing the module. Unfortunately this is only possible
-- when the GHC version matches the version that HLint expects. Otherwise we
-- have to let HLint re-parse the module to produce it's expected AST.
--
-- This module is responsible for picking that behavior. The so-called "native"
-- behavior is the one where GHC's parse module is re-used. The "fallback"
-- parser is the one that re-parses the module.
--
-- Doing this requires a build-time decision, which means using CPP. Therefore
-- this module is kept as small as possible in order to make it easier to
-- reason about.
--
-- It is expected that both the "native" and "fallback" parsers expose the
-- exact same interface.
--
-- You could in theory always use the "fallback" parser, but that would mean
-- doing a lot of unnecessary work by re-parsing modules. Parsing is typically
-- fast and this is hard to quantify, so if you want to go that route you
-- should consider using Ollie Charles's hlint-source-plugin instead.
-- <https://github.com/ocharles/hlint-source-plugin>
module Splint.Parser
  ( parse
  )
where

#if (__GLASGOW_HASKELL__ == 810)
import Splint.Parser.Native (parse)
#else
import Splint.Parser.Fallback (parse)
#endif
