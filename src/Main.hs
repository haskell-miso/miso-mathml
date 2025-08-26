-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Mathml
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
import           Miso.String
-----------------------------------------------------------------------------
#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = run $ startApp vcomp
#ifndef WASM
  { styles = [ Href "assets/style.css" ]
  }
#endif
  where
    vcomp = component () noop $ \() ->
      div_
        [class_ "container"]
        [ githubStar
        , header_
            []
            [ h1_ [] ["Mathematical Expressions with MathML and miso 🍜 "]
            , p_
                [class_ "subtitle"]
                [ "Modern examples of mathematical notation on the web"
                ]
            ]
        , div_
            [class_ "math-card"]
            [ h2_ [class_ "math-title"] ["Quadratic Formula"]
            , div_
                [class_ "math-container"]
                [ math_
                    [ display_ "block"
                    , xmlns_ "http://www.w3.org/1998/Math/MathML"
                    , class_ "math-equation"
                    ]
                    [ mi_ [] ["x"]
                    , mo_ [] ["="]
                    , mfrac_
                        []
                        [ mrow_
                            []
                            [ mo_ [form_ "prefix"] ["−"]
                            , mi_ [] ["b"]
                            , mo_ [] ["±"]
                            , msqrt_
                                []
                                [ msup_ [] [mi_ [] ["b"], mn_ [] ["2"]]
                                , mo_ [] ["−"]
                                , mn_ [] ["4"]
                                , mi_ [] ["a"]
                                , mi_ [] ["c"]
                                ]
                            ]
                        , mrow_ [] [mn_ [] ["2"], mi_ [] ["a"]]
                        ]
                    ]
                ]
            , p_
                [class_ "explanation"]
                [ "The quadratic formula solves equations of the form ax² + bx + c = 0"
                ]
            ]
        , div_
            [class_ "math-card"]
            [ h2_ [class_ "math-title"] ["Pythagorean Theorem"]
            , div_
                [class_ "math-container"]
                [ math_
                    [ display_ "block"
                    , xmlns_ "http://www.w3.org/1998/Math/MathML"
                    , class_ "math-equation"
                    ]
                    [ msup_ [] [mi_ [] ["a"], mn_ [] ["2"]]
                    , mo_ [] ["+"]
                    , msup_ [] [mi_ [] ["b"], mn_ [] ["2"]]
                    , mo_ [] ["="]
                    , msup_ [] [mi_ [] ["c"], mn_ [] ["2"]]
                    ]
                ]
            , p_
                [class_ "explanation"]
                [ "In a right triangle, the square of the hypotenuse is equal to the sum of the squares of the other two sides"
                ]
            ]
        , div_
            [class_ "math-card"]
            [ h2_ [class_ "math-title"] ["Integral Example"]
            , div_
                [class_ "math-container"]
                [ math_
                    [ display_ "block"
                    , xmlns_ "http://www.w3.org/1998/Math/MathML"
                    , class_ "math-equation"
                    ]
                    [ msubsup_ [] [mo_ [] ["∫"], mi_ [] ["a"], mi_ [] ["b"]]
                    , mi_ [] ["f"]
                    , mo_ [stretchy_ False] ["("]
                    , mi_ [] ["x"]
                    , mo_ [stretchy_ False] [")"]
                    , mi_ [] ["d"]
                    , mi_ [] ["x"]
                    , mo_ [] ["="]
                    , mi_ [] ["F"]
                    , mo_ [stretchy_ False] ["("]
                    , mi_ [] ["b"]
                    , mo_ [stretchy_ False] [")"]
                    , mo_ [] ["−"]
                    , mi_ [] ["F"]
                    , mo_ [stretchy_ False] ["("]
                    , mi_ [] ["a"]
                    , mo_ [stretchy_ False] [")"]
                    ]
                ]
            , p_
                [class_ "explanation"]
                [ "The fundamental theorem of calculus: the definite integral of a function can be computed using its antiderivative"
                ]
            ]
        , div_
            [class_ "math-card"]
            [ h2_ [class_ "math-title"] ["Matrix Example"]
            , div_
                [class_ "math-container"]
                [ math_
                    [ display_ "block"
                    , xmlns_ "http://www.w3.org/1998/Math/MathML"
                    , class_ "math-equation"
                    ]
                    [ mi_ [] ["A"]
                    , mo_ [] ["="]
                    , mfenced_
                        [ close_ "]"
                        , open_ "["
                        ]
                        [ mtable_
                            []
                            [ mtr_
                                []
                                [ mtd_ [] [mn_ [] ["1"]]
                                , mtd_ [] [mn_ [] ["2"]]
                                , mtd_ [] [mn_ [] ["3"]]
                                ]
                            , mtr_
                                []
                                [ mtd_ [] [mn_ [] ["4"]]
                                , mtd_ [] [mn_ [] ["5"]]
                                , mtd_ [] [mn_ [] ["6"]]
                                ]
                            , mtr_
                                []
                                [ mtd_ [] [mn_ [] ["7"]]
                                , mtd_ [] [mn_ [] ["8"]]
                                , mtd_ [] [mn_ [] ["9"]]
                                ]
                            ]
                        ]
                    ]
                ]
            , p_
                [class_ "explanation"]
                [ "A 3×3 matrix example showing how matrices can be represented in MathML"
                ]
            ]
        , footer_
            []
            [ p_
                []
                [ "Modern MathML Example | Built with pure HTML and CSS"
                ]
            ]
        ]
-----------------------------------------------------------------------------
mfenced_ :: [Attribute action] -> [View model action] -> View model action
mfenced_ = nodeMathml "mfenced"
-----------------------------------------------------------------------------
close_ :: MisoString -> Attribute action
close_ = textProp "close"
-----------------------------------------------------------------------------
open_ :: MisoString -> Attribute action
open_ = textProp "open"
-----------------------------------------------------------------------------
githubStar :: View model action
githubStar = H.iframe_
    [ P.title_ "GitHub"
    , P.height_ "30"
    , P.width_ "170"
    , textProp "scrolling" "0"
    , textProp "frameborder" "0"
    , P.src_
      "https://ghbtns.com/github-btn.html?user=haskell-miso&repo=miso-mathml&type=star&count=true&size=large"
    ]
    []
-----------------------------------------------------------------------------
