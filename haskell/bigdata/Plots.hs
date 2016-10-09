module Plots where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

main = toFile def "example.png" $ do
    layout_title .= "Amplitude Modulation"
    plot (line "am" [signal [0,(0.5)..400]])
    plot (points "am points" (signal [0,7..400]))

doPlot :: FilePath -> String -> String -> [(Double, Double)] -> IO ()
doPlot fileName title seriesName xy = do
    toFile def fileName $ do
        layout_title .= title
        plot $ points seriesName xy
