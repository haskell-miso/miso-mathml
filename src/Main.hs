-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Mathml
import           Miso.Mathml.Property
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
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
      H.div_
        [P.class_ "container"]
        [ githubStar
        , H.header_
            []
            [ H.h1_ [] ["Mathematical Expressions with MathML and miso 🍜 "]
            , H.p_
                [P.class_ "subtitle"]
                [ "Modern examples of mathematical notation on the web"
                ]
            ]
        , H.div_
            [P.class_ "math-card"]
            [ H.h2_ [P.class_ "math-title"] ["Quadratic Formula"]
            , H.div_
                [P.class_ "math-container"]
                [ math_
                    [ display_ "block"
                    , P.xmlns_ "http://www.w3.org/1998/Math/MathML"
                    , P.class_ "math-equation"
                    ]
                    [ mi_ [] ["x"]
                    , mo_ [] ["="]
                    , mfrac_
                        []
                        [ mrow_
                            []
                            [ mo_ [P.form_ "prefix"] ["−"]
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
            , H.p_
                [P.class_ "explanation"]
                [ "The quadratic formula solves equations of the form ax² + bx + c = 0"
                ]
            ]
        , H.div_
            [P.class_ "math-card"]
            [ H.h2_ [P.class_ "math-title"] ["Pythagorean Theorem"]
            , H.div_
                [P.class_ "math-container"]
                [ math_
                    [ display_ "block"
                    , P.xmlns_ "http://www.w3.org/1998/Math/MathML"
                    , P.class_ "math-equation"
                    ]
                    [ msup_ [] [mi_ [] ["a"], mn_ [] ["2"]]
                    , mo_ [] ["+"]
                    , msup_ [] [mi_ [] ["b"], mn_ [] ["2"]]
                    , mo_ [] ["="]
                    , msup_ [] [mi_ [] ["c"], mn_ [] ["2"]]
                    ]
                ]
            , H.p_
                [P.class_ "explanation"]
                [ "In a right triangle, the square of the hypotenuse is equal to the sum of the squares of the other two sides"
                ]
            ]
        , H.div_
            [P.class_ "math-card"]
            [ H.h2_ [P.class_ "math-title"] ["Integral Example"]
            , H.div_
                [P.class_ "math-container"]
                [ math_
                    [ display_ "block"
                    , P.xmlns_ "http://www.w3.org/1998/Math/MathML"
                    , P.class_ "math-equation"
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
            , H.p_
                [P.class_ "explanation"]
                [ "The fundamental theorem of calculus: the definite integral of a function can be computed using its antiderivative"
                ]
            ]
        , H.div_
            [P.class_ "math-card"]
            [ H.h2_ [P.class_ "math-title"] ["Matrix Example"]
            , H.div_
                [P.class_ "math-container"]
                [ math_
                    [ display_ "block"
                    , P.xmlns_ "http://www.w3.org/1998/Math/MathML"
                    , P.class_ "math-equation"
                    ]
                    [ mi_ [] ["A"]
                    , mo_ [] ["="]
                    , mrow_
                      []
                        [ mo_ [] [ "[" ]
                        , mtable_
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
                        , mo_ [] [ "]" ]
                        ]
                    ]
                ]
            , H.p_
                [P.class_ "explanation"]
                [ "A 3×3 matrix example showing how matrices can be represented in MathML"
                ]
            ]
        , H.footer_
            []
            [ H.p_
                []
                [ "Modern MathML Example | Built with pure HTML and CSS"
                ]
            ]
        ]
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
