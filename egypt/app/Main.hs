module Main (main) where
   
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,x^2) | x <- xs ]

main = toFile def "example1_big.png" $ do
    layout_title .= "Parabola"
    setColors [opaque blue, opaque red]
    plot (line "am" [signal [(-20),(-19.5)..20]])
    plot (points "am points" (signal [(-20),(-15)..20]))
