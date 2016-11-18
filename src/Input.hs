module Input
  ( inputWire
  , InputMap
  , PlayerInput
  , PlayerKey(..)
  , isDown
  , adInputMap
  , fgInputMap
  , arrowsInputMap
  , parseEvents
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import qualified SDL
import qualified SDL.Input.Keyboard.Codes as Key

import           Control.Wire             (Wire, mkGen_, (.))

import           Prelude                  hiding (id, (.))

type PlayerInput = Set PlayerKey

data PlayerKey = KeyLeft | KeyRight deriving (Eq, Ord, Show)

type InputMap = Map Key.Scancode PlayerKey

adInputMap :: InputMap
adInputMap =
  Map.fromList [ (Key.ScancodeA, KeyLeft)
               , (Key.ScancodeD, KeyRight)
               ]


arrowsInputMap :: InputMap
arrowsInputMap =
  Map.fromList [ (Key.ScancodeLeft, KeyLeft)
               , (Key.ScancodeRight, KeyRight)
               ]


fgInputMap :: InputMap
fgInputMap =
  Map.fromList [ (Key.ScancodeF, KeyLeft)
               , (Key.ScancodeG, KeyRight)
               ]



-- getEvents :: IO [SDL.Event]
-- getEvents = SDL.pollEventskj

-- Wire that fetches SDL keyboard events & outputs a set containing all pressed keys
-- for a given inputmap
inputWire :: InputMap -> Wire s e IO PlayerInput PlayerInput
inputWire imap = mkGen_ $ fmap Right . (getEvents imap)

-- isDown :: PlayerInput -> PlayerKey -> Bool
-- isDown = flip Set.member
isDown :: PlayerKey -> PlayerInput -> Bool
isDown = Set.member

mapInput :: InputMap -> SDL.Keysym -> Maybe PlayerKey
mapInput imap ks = Map.lookup (SDL.keysymScancode ks) imap


-- SDL event helper functions
parseEvents :: InputMap -> PlayerInput -> [SDL.Event] -> PlayerInput
parseEvents imap =
  foldr
    (\ev s ->
       case SDL.eventPayload ev of
         SDL.KeyboardEvent (SDL.KeyboardEventData _ m _ ks) ->
           case mapInput imap ks of
             Nothing -> s
             Just ks' -> case m of
              SDL.Pressed -> Set.insert ks' s
              SDL.Released -> Set.delete ks' s
         _ -> s)


getEvents :: InputMap -> PlayerInput -> IO PlayerInput
getEvents imap oldKeys = parseEvents imap oldKeys <$> SDL.pollEvents
