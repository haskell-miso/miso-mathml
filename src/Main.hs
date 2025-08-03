-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import Miso
import Miso.Mathml
-----------------------------------------------------------------------------
#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = run $ startApp $ component () noop $ \() ->
  math_
    [display_ "block"]
    [ mrow_ []
      [ msub_ []
        [ mi_ [] [text "x"]
        , mtext_ [] [text "1,2"]
        ]
      , mo_ [] [text "="]
      , mfrac_ []
        [ mrow_ []
          [ mo_ [] [text "−"]
          , mi_ [] [text "b"]
          , mo_ [] [text "±"]
          , msqrt_
            []
            [ mrow_ []
              [ msup_ []
                [ mi_ [] [text "b"]
                , mn_ [] [text "2"]
                ]
              , mo_ [] [text "−"]
              , mrow_ []
                [ mn_ [] [text "4"]
                , mi_ [] [text "a"]
                , mi_ [] [text "c"]
                ]
              ]
            ]
          ]
        , mrow_ []
          [ mn_ [] [text "2"]
          , mi_ [] [text "a"]
          ]
        ]
      ]
    ]
-----------------------------------------------------------------------------
