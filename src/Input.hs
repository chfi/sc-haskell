module Input (inputWire, PlayerInput, PlayerKeys(..), isDown) where



import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import qualified SDL
import qualified SDL.Input.Keyboard.Codes as Key

import           Control.Wire             (Wire, mkGen_, (.))

import           Prelude                  hiding (id, (.))

type PlayerInput = Set PlayerKeys

data PlayerKeys = KeyUp | KeyRight | KeyDown | KeyLeft deriving (Eq, Show)


-- Wire that fetches SDL keyboard events & outputs a set containing all pressed keys
inputWire :: Wire s e IO (Set SDL.Keysym) (Set SDL.Keysym)
inputWire = mkGen_ $ fmap Right . getEvents


-- SDL event helper functions
parseEvents :: Set SDL.Keysym -> [SDL.Event] -> Set SDL.Keysym
parseEvents =
    foldr
        (\ev s ->
              case SDL.eventPayload ev of
                  SDL.KeyboardEvent (SDL.KeyboardEventData _ m _ ks) ->
                      case m of
                          SDL.Pressed -> Set.insert ks s
                          SDL.Released -> Set.delete ks s
                  _ -> s)

getEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
getEvents oldKeys = parseEvents oldKeys <$> SDL.pollEvents


isDown :: SDL.Scancode -> Set SDL.Keysym -> Bool
isDown sc = not . Set.null . Set.filter ((== sc) . SDL.keysymScancode)


isKeyDown' :: PlayerKeys -> PlayerInput -> Bool
isKeyDown' keys = not . Set.null . Set.filter (== keys)
