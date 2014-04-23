{-# LANGUAGE TemplateHaskell #-}

import Keyword

$(return [keyword "Fred"])

main = print Fred

