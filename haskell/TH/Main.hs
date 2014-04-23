{-# LANGUAGE TemplateHaskell #-}

import Keyword


$(return [keyword "Fred"])

f2 = "Fred2"

$(return [keyword f2])

main = print Fred

