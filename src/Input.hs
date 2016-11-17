module Input
  ( inputWire
  , PlayerInput
  , PlayerKey(..)
  , isDown
  , player1InputMap
  , player2InputMap
  , player3InputMap
  ) where



import Data.Maybe (fromJust)

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


-- newtype InputMap =
--   InputMap { leftKey :: SDL.Keysym
--            , rightKey :: SDL.Keysym
--            }
-- createInputMap :: SDL.Keysym
--                -> SDL.Keysym
--                ->

-- type InputMap = Map SDL.Keysym PlayerKey

type InputMap = Map Key.Scancode PlayerKey


player1InputMap :: InputMap
player1InputMap =
  Map.fromList [ (Key.ScancodeA, KeyLeft)
               , (Key.ScancodeD, KeyRight)
               ]


player2InputMap :: InputMap
player2InputMap =
  Map.fromList [ (Key.ScancodeLeft, KeyLeft)
               , (Key.ScancodeRight, KeyRight)
               ]


player3InputMap :: InputMap
player3InputMap =
  Map.fromList [ (Key.ScancodeJ, KeyLeft)
               , (Key.ScancodeL, KeyRight)
               ]


-- Wire that fetches SDL keyboard events & outputs a set containing all pressed keys
-- for a given inputmap
inputWire :: InputMap -> Wire s e IO PlayerInput PlayerInput
inputWire imap = mkGen_ $ fmap Right . (getEvents imap)

isDown :: PlayerInput -> PlayerKey -> Bool
isDown = flip Set.member

mapInput :: InputMap -> SDL.Keysym -> PlayerKey
mapInput imap ks = fromJust $ Map.lookup (SDL.keysymScancode ks) imap


-- SDL event helper functions
parseEvents :: InputMap -> PlayerInput -> [SDL.Event] -> PlayerInput
parseEvents imap =
  foldr
    (\ev s ->
       case SDL.eventPayload ev of
         SDL.KeyboardEvent (SDL.KeyboardEventData _ m _ ks) ->
           case m of
             SDL.Pressed -> Set.insert (mapInput imap ks) s
             SDL.Released -> Set.delete (mapInput imap ks) s
         _ -> s)


getEvents :: InputMap -> PlayerInput -> IO PlayerInput
getEvents imap oldKeys = parseEvents imap oldKeys <$> SDL.pollEvents
