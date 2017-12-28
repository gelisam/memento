{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Data.Text

import MementoSimpleFLTK


data AppState = AppState
  { appStateGoingUp :: Bool
  , appStateCount   :: Int
  }
  deriving Show

initialState :: AppState
initialState = AppState True 0


data Event
  = ToggleDirection
  | BumpCount
  deriving (Eq, Show)

toggleDirection :: AppState -> AppState
toggleDirection appState@AppState{..} = appState
                                      { appStateGoingUp = not appStateGoingUp }

bumpCount :: AppState -> AppState
bumpCount appState@AppState{..} = appState
                                { appStateCount = if appStateGoingUp
                                                  then appStateCount + 1
                                                  else appStateCount - 1
                                }

update :: Event -> AppState -> AppState
update ToggleDirection = toggleDirection
update BumpCount       = bumpCount


windowSize :: Size
windowSize = Size (Width 130) (Height 90)

render :: AppState -> Pair (Button Event)
render AppState{..} = Pair (Button (Rectangle (Position (X 30) (Y 30))
                                              (Size (Width 30) (Height 30)))
                                   (if appStateGoingUp then "^" else "v")
                                   ToggleDirection)
                           (Button (Rectangle (Position (X 70) (Y 30))
                                              (Size (Width 30) (Height 30)))
                                   (pack . show $ appStateCount)
                                   BumpCount)

main :: IO ()
main = runMementoSimpleFLTK initialState update windowSize render
