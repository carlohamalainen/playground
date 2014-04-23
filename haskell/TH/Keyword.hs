{-# LANGUAGE TemplateHaskell #-}

-- http://stackoverflow.com/questions/5722523/local-variables-in-template-haskell-declarations

-- http://pozorvlak.livejournal.com/95054.html

module Keyword (keyword) where

import Language.Haskell.TH.Syntax
-- keyword name = [d| data $(name) = $(name) |]

-- DataD Cxt Name [Name] [Con] [Name]
-- data Con = NormalC Name [StrictType] | ...
keyword name = DataD [] name' [] [NormalC name' []] (map mkName ["Show", "Eq"])
        where name' = mkName name

